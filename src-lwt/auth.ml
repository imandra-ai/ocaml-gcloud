(** Credential discovery and token fetching on Lwt. The types and pure
    helpers come from {!Gcloud.Auth}. *)

include Gcloud.Auth
module L = (val Logs_lwt.src_log src)

let ok = Lwt_result.ok

module Metadata_lwt = struct
  open Compute_engine.Metadata

  let ping () =
    let uri = Uri.of_string metadata_ip_root in
    let open Lwt.Infix in
    Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers >>= Util.drain_body

  let get_project_id () :
      ( string,
        [> `Bad_GCE_metadata_response of Cohttp.Code.status_code ] )
      Lwt_result.t =
    let open Lwt.Infix in
    let uri =
      Uri.of_string (Printf.sprintf "%s/project/project-id" metadata_root)
    in
    Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers
    >>= Util.consume_body
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt_result.return body
    | status -> `Bad_GCE_metadata_response status |> Lwt_result.fail
end

let token_info_mvar : token_info option Lwt_mvar.t = Lwt_mvar.create None

let credentials_of_file (credentials_file : string) :
    (credentials, [> `No_credentials | `Bad_credentials_format ]) result Lwt.t =
  let open Lwt.Syntax in
  let* () =
    L.debug (fun m -> m "Looking for credentials file: %s" credentials_file)
  in
  let* exists = Lwt_unix.file_exists credentials_file in
  if not exists then
    let* () = L.debug (fun m -> m "Not found") in
    Lwt.return_error `No_credentials
  else
    let* () = L.debug (fun m -> m "Found") in
    Lwt_io.(with_file ~mode:input) credentials_file (fun input_chan ->
        let* lines = Lwt_io.read_lines input_chan |> Lwt_stream.to_list in
        lines |> String.concat "\n" |> credentials_of_string |> Lwt.return)

let access_token_of_credentials (scopes : string list)
    (credentials : credentials) :
    (Access_token.t, [> `Bad_token_response of string ]) result Lwt.t =
  let open Lwt_result.Syntax in
  match credentials with
  | Authorized_user c ->
      let token_uri =
        Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
          ~path:"oauth2/v4/token"
      in
      let params =
        [
          ("client_id", [ c.client_id ]);
          ("client_secret", [ c.client_secret ]);
          ("refresh_token", [ c.refresh_token ]);
          ("grant_type", [ "refresh_token" ]);
        ]
      in
      let* res =
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post_form token_uri ~params
        >>= Util.consume_body |> ok
      in
      access_token_of_response ~of_json:access_token_of_json res |> Lwt.return
  | Service_account c -> (
      let now = Unix.time () in
      let* key =
        Cstruct.of_string c.private_key
        |> X509.Private_key.decode_pem
        |> CCResult.map_err (function `Msg msg -> `Bad_credentials_priv_key msg)
        |> Lwt.return
      in
      match key with
      | `RSA priv_key ->
          let jwk = Jose.Jwk.make_priv_rsa priv_key in
          let header = Jose.Header.make_header ~typ:"JWT" jwk in
          let payload =
            Jose.Jwt.empty_payload
            |> Jose.Jwt.add_claim "iss" (`String c.client_email)
            |> Jose.Jwt.add_claim "scope" (`String (String.concat " " scopes))
            |> Jose.Jwt.add_claim "aud" (`String c.token_uri)
            |> Jose.Jwt.add_claim "iat" (`String (Printf.sprintf "%.0f" now))
            |> Jose.Jwt.add_claim "exp"
                 (`String (Printf.sprintf "%.0f" (now +. 3600.)))
          in
          let* jwt =
            Jose.Jwt.sign ~header ~payload jwk
            |> CCResult.map_err (function `Msg msg -> `Jwt_signing_error msg)
            |> Lwt.return
          in
          let params =
            [
              ("grant_type", [ "urn:ietf:params:oauth:grant-type:jwt-bearer" ]);
              ("assertion", [ Jose.Jwt.to_string jwt ]);
            ]
          in
          let* res =
            let open Lwt.Infix in
            Cohttp_lwt_unix.Client.post_form (Uri.of_string c.token_uri) ~params
            >>= Util.consume_body |> ok
          in
          access_token_of_response ~of_json:access_token_of_json res
          |> Lwt.return
      | _ -> Lwt_result.fail (`Bad_credentials_priv_key "Not RSA key"))
  | GCE_metadata _ ->
      let uri =
        Printf.sprintf "%s/instance/service-accounts/default/token"
          Compute_engine.Metadata.metadata_root
        |> Uri.of_string
      in
      let* res =
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get uri
          ~headers:Compute_engine.Metadata.metadata_headers
        >>= Util.consume_body |> ok
      in
      access_token_of_response ~of_json:access_token_of_json res |> Lwt.return
  | External_account (c : External_account_credentials.t) -> (
      (* Only tested against Workload Identity Federation via Github Actions. The flow is:
           - Fetch Github token ("subject token") via the details in [credentials_source]
           - Exchange the token for a gcloud one via a gcloud endpoint
           - Once authed via the new token, perform service account impersonation via another endpoint.

         To perform service account impersonation, the IAM scope is required (this is also required on token refresh).
      *)
      let scopes = [ Scopes.iam ] @ scopes in
      let* res =
        let* () = L.debug (fun m -> m "Requesting subject token") |> ok in
        let* subject_token =
          let subject_token_uri = Uri.of_string c.credential_source.url in
          let* resp =
            let open Lwt.Infix in
            Cohttp_lwt_unix.Client.get
              ~headers:(Cohttp.Header.of_list c.credential_source.headers)
              subject_token_uri
            >>= Util.consume_body |> ok
          in
          External_account_credentials.subject_token_of_response c resp
          |> Lwt.return
        in
        let* () = L.debug (fun m -> m "Performing token exchange") |> ok in
        let token_uri = Uri.of_string c.token_url in
        let params =
          `Assoc
            [
              ( "grantType",
                `String "urn:ietf:params:oauth:grant-type:token-exchange" );
              ("audience", `String c.audience);
              ("scope", `String (scopes |> CCString.concat " "));
              ( "requestedTokenType",
                `String "urn:ietf:params:oauth:token-type:access_token" );
              ("subjectToken", `String subject_token);
              ("subjectTokenType", `String c.subject_token_type);
            ]
        in
        let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string params) in
        let* res =
          let open Lwt.Infix in
          Cohttp_lwt_unix.Client.post token_uri ~body
          >>= Util.consume_body |> ok
        in
        Lwt_result.return res
      in
      match c.service_account_impersonation_url with
      | None ->
          access_token_of_response ~of_json:access_token_of_json res
          |> Lwt.return
      | Some sac ->
          let* () =
            L.debug (fun m -> m "attempting to impersonate service account")
            |> ok
          in
          let* initial_access_token =
            access_token_of_response res |> Lwt.return
          in
          let headers =
            Cohttp.Header.of_list
              [
                ( "Authorization",
                  Printf.sprintf "Bearer %s" initial_access_token.access_token
                );
              ]
          in
          let params =
            `Assoc
              [ ("scope", `List (scopes |> CCList.map (fun s -> `String s))) ]
          in
          let body =
            Cohttp_lwt.Body.of_string (Yojson.Basic.to_string params)
          in
          let uri = Uri.of_string sac in
          let* () = L.debug (fun m -> m "POST %a" Uri.pp_hum uri) |> ok in
          let* res =
            let open Lwt.Infix in
            Cohttp_lwt_unix.Client.post uri ~headers ~body
            >>= Util.consume_body |> ok
          in
          access_token_of_response ~of_json:impersonated_access_token_of_json
            res
          |> Lwt.return)

let discover_credentials_with (discovery_mode : discovery_mode) =
  let open Lwt.Syntax in
  let* () =
    L.debug (fun m ->
        m "Attempting authentication using %a" pp_discovery_mode discovery_mode)
  in
  match discovery_mode with
  | Discover_credentials_path_from_env -> (
      let credentials_file =
        Sys.getenv_opt Environment_vars.google_application_credentials
      in
      match credentials_file with
      | None -> Lwt.return_error `No_credentials
      | Some credentials_file -> credentials_of_file credentials_file)
  | Discover_credentials_json_from_env -> (
      let credentials_json =
        Sys.getenv_opt Environment_vars.google_application_credentials_json
      in
      match credentials_json with
      | None -> Lwt.return_error `No_credentials
      | Some json_str -> credentials_of_string json_str |> Lwt.return)
  | Discover_credentials_from_cloud_sdk_path ->
      credentials_of_file Paths.application_default_credentials
  | Discover_credentials_from_gce_metadata ->
      let ping =
        Lwt.catch
          (fun () ->
            let open Lwt_result.Syntax in
            let* resp = Metadata_lwt.ping () |> ok in
            let* () = L.debug (fun m -> m "Got metadata response") |> ok in
            let has_metadata_header =
              Compute_engine.Metadata.response_has_metadata_header resp
            in
            match Cohttp.Response.status resp with
            | `OK when has_metadata_header ->
                let* () =
                  L.debug (fun m -> m "Metadata response was ok with header")
                  |> ok
                in
                let* project_id = Metadata_lwt.get_project_id () in
                Lwt_result.return (GCE_metadata { project_id })
            | code ->
                let* () =
                  L.debug (fun m ->
                      m "Metadata response was: (%d, header: %b)"
                        (Cohttp.Code.code_of_status code)
                        has_metadata_header)
                  |> ok
                in

                Lwt.return_error `No_credentials)
          (fun exn ->
            let* () =
              L.debug (fun m ->
                  let s = Printexc.to_string exn in
                  m "Exception while pinging metadata endpoint: %s" s)
            in
            Lwt.return_error `No_credentials)
      in
      let timeout =
        let* () =
          Lwt_unix.sleep Compute_engine.Metadata.metadata_default_timeout
        in
        Lwt.return_error `No_credentials
      in
      Lwt.pick [ ping; timeout ]

let rec first_ok ~(error : 'e) (fs : (unit -> ('a, 'e) result Lwt.t) list) :
    ('a, 'e) result Lwt.t =
  match fs with
  | [] -> Lwt.return_error error
  | t :: fs -> (
      let open Lwt.Infix in
      t () >>= function
      | Ok x -> Lwt.return_ok x
      | Error error -> first_ok ~error fs)

let discover_credentials () : (credentials, [> error ]) Lwt_result.t =
  [
    Discover_credentials_path_from_env;
    Discover_credentials_json_from_env;
    Discover_credentials_from_cloud_sdk_path;
    Discover_credentials_from_gce_metadata;
  ]
  |> List.map (fun discovery_mode () ->
         let open Lwt.Syntax in
         let* result = discover_credentials_with discovery_mode in
         match result with
         | Ok x ->
             let* () =
               L.debug (fun m ->
                   m "Success for discovery mode %a" pp_discovery_mode
                     discovery_mode)
             in
             Lwt_result.return x
         | Error (#error as e) ->
             let* () =
               L.debug (fun m ->
                   m "Error for discovery mode %a: %a" pp_discovery_mode
                     discovery_mode pp_error e)
             in
             Lwt_result.fail e
         | Error e ->
             let* () = L.debug (fun m -> m "Unknown error") in
             Lwt_result.fail e)
  |> first_ok ~error:`No_credentials

let get_access_token ?(scopes : string list = []) () :
    (token_info, [> error ]) Lwt_result.t =
  let get_new_access_token scopes =
    let open Lwt_result.Syntax in
    let* credentials = discover_credentials () in
    let* access_token = access_token_of_credentials scopes credentials in
    let+ () = L.info (fun m -> m "Authenticated OK!") |> ok in
    let scopes =
      CCList.union ~eq:String.equal scopes
        access_token.additional_refresh_scopes
    in
    { credentials; token = access_token; created_at = Unix.time (); scopes }
  in
  let has_requested_scopes token_info =
    CCList.subset ~eq:String.equal scopes token_info.scopes
  in
  let is_expired token_info =
    Unix.time ()
    > token_info.created_at +. float_of_int token_info.token.expires_in -. 30.
  in
  let open Lwt.Syntax in
  let* token_info = Lwt_mvar.take token_info_mvar in
  let* token_info_result =
    match token_info with
    | Some token_info
      when has_requested_scopes token_info && not (is_expired token_info) ->
        Lwt.return_ok token_info
    | Some token_info ->
        let* () =
          if is_expired token_info then
            L.debug (fun m -> m "Re-authenticating: Token is expired")
          else
            L.debug (fun m ->
                m "Re-authenticating: Token does not have required scopes")
        in
        get_new_access_token
          (CCList.union ~eq:String.equal token_info.scopes scopes)
    | None -> get_new_access_token scopes
  in
  let* () = Lwt_mvar.put token_info_mvar (CCResult.to_opt token_info_result) in
  Lwt.return token_info_result
