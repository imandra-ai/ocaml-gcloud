(** Credential discovery and token fetching in direct style. The types and
    pure helpers come from {!Gcloud.Auth}.

    Supported credentials: service account key files, authorized-user
    (gcloud ADC) refresh tokens, and the GCE metadata server. External-account
    (workload identity federation) credentials are not ported; use
    [gcloud-lwt] for those. *)

include Gcloud.Auth

module Metadata = struct
  open Compute_engine.Metadata

  let timeout_s = metadata_default_timeout

  let ping () : Ezcurl.response =
    Http_ezcurl.http ~timeout_s ~meth:`GET ~headers:metadata_headers
      (Uri.of_string metadata_ip_root)

  let has_metadata_header (r : Ezcurl.response) =
    List.exists
      (fun (k, v) ->
        String.equal (String.lowercase_ascii k) metadata_flavor_header
        && String.equal (String.trim v) metadata_flavor_value)
      r.Ezcurl.headers

  let get_project_id () :
      ( string,
        [> `Bad_GCE_metadata_response of Cohttp.Code.status_code ] )
      result =
    let status, body =
      Http_ezcurl.call ~timeout_s ~meth:`GET ~headers:metadata_headers
        (Uri.of_string (Printf.sprintf "%s/project/project-id" metadata_root))
    in
    match status with
    | `OK -> Ok body
    | status -> Stdlib.Error (`Bad_GCE_metadata_response status)
end

let credentials_of_file (credentials_file : string) :
    (credentials, [> `No_credentials | `Bad_credentials_format ]) result =
  Log.debug (fun m -> m "Looking for credentials file: %s" credentials_file);
  if not (Sys.file_exists credentials_file) then (
    Log.debug (fun m -> m "Not found");
    Stdlib.Error `No_credentials)
  else (
    Log.debug (fun m -> m "Found");
    CCIO.with_in credentials_file CCIO.read_all |> credentials_of_string)

let post_form (uri : Uri.t) (params : (string * string list) list) :
    Cohttp.Response.t * string =
  let headers =
    Cohttp.Header.of_list
      [ ("Content-Type", "application/x-www-form-urlencoded") ]
  in
  let status, body =
    Http_ezcurl.call ~meth:`POST ~headers
      ~body:(Uri.encoded_of_query params)
      uri
  in
  (Cohttp.Response.make ~status (), body)

let access_token_of_credentials (scopes : string list)
    (credentials : credentials) : (Access_token.t, [> error ]) result =
  let open CCResult.Infix in
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
      post_form token_uri params |> access_token_of_response
  | Service_account c -> (
      let now = Unix.time () in
      let* key =
        Cstruct.of_string c.private_key
        |> X509.Private_key.decode_pem
        |> CCResult.map_err (function `Msg msg -> `Bad_credentials_priv_key msg)
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
          in
          let params =
            [
              ("grant_type", [ "urn:ietf:params:oauth:grant-type:jwt-bearer" ]);
              ("assertion", [ Jose.Jwt.to_string jwt ]);
            ]
          in
          post_form (Uri.of_string c.token_uri) params
          |> access_token_of_response
      | _ -> Stdlib.Error (`Bad_credentials_priv_key "Not RSA key"))
  | GCE_metadata _ ->
      let uri =
        Printf.sprintf "%s/instance/service-accounts/default/token"
          Compute_engine.Metadata.metadata_root
        |> Uri.of_string
      in
      let status, body =
        Http_ezcurl.call ~meth:`GET
          ~headers:Compute_engine.Metadata.metadata_headers uri
      in
      access_token_of_response (Cohttp.Response.make ~status (), body)
  | External_account _ ->
      Stdlib.Error
        (`Bad_token_response
          "external_account credentials are not supported by \
           Gcloud_direct.Auth (use gcloud-lwt)")

let discover_credentials_with (discovery_mode : discovery_mode) :
    (credentials, [> error ]) result =
  Log.debug (fun m ->
      m "Attempting authentication using %a" pp_discovery_mode discovery_mode);
  match discovery_mode with
  | Discover_credentials_path_from_env -> (
      match Sys.getenv_opt Environment_vars.google_application_credentials with
      | None -> Stdlib.Error `No_credentials
      | Some credentials_file -> credentials_of_file credentials_file)
  | Discover_credentials_json_from_env -> (
      match
        Sys.getenv_opt Environment_vars.google_application_credentials_json
      with
      | None -> Stdlib.Error `No_credentials
      | Some json_str -> credentials_of_string json_str)
  | Discover_credentials_from_cloud_sdk_path ->
      credentials_of_file Paths.application_default_credentials
  | Discover_credentials_from_gce_metadata -> (
      try
        let resp = Metadata.ping () in
        Log.debug (fun m -> m "Got metadata response");
        let has_metadata_header = Metadata.has_metadata_header resp in
        match Cohttp.Code.status_of_code resp.Ezcurl.code with
        | `OK when has_metadata_header ->
            Log.debug (fun m -> m "Metadata response was ok with header");
            let open CCResult.Infix in
            let+ project_id = Metadata.get_project_id () in
            GCE_metadata { project_id }
        | _ ->
            Log.debug (fun m ->
                m "Metadata response was: (%d, header: %b)" resp.Ezcurl.code
                  has_metadata_header);
            Stdlib.Error `No_credentials
      with exn ->
        Log.debug (fun m ->
            m "Exception while pinging metadata endpoint: %s"
              (Printexc.to_string exn));
        Stdlib.Error `No_credentials)

let discover_credentials () : (credentials, [> error ]) result =
  let rec first_ok ~error = function
    | [] -> Stdlib.Error error
    | discovery_mode :: rest -> (
        match discover_credentials_with discovery_mode with
        | Ok x ->
            Log.debug (fun m ->
                m "Success for discovery mode %a" pp_discovery_mode
                  discovery_mode);
            Ok x
        | Stdlib.Error e ->
            Log.debug (fun m ->
                m "Error for discovery mode %a: %a" pp_discovery_mode
                  discovery_mode pp_error e);
            first_ok ~error:e rest)
  in
  first_ok ~error:`No_credentials
    [
      Discover_credentials_path_from_env;
      Discover_credentials_json_from_env;
      Discover_credentials_from_cloud_sdk_path;
      Discover_credentials_from_gce_metadata;
    ]

(* Token cache shared by all threads and domains. The lock is held across the
   refresh so concurrent callers wait for one exchange rather than each
   performing their own, mirroring the Lwt backend's mvar. *)
let token_cache : token_info option ref = ref None
let token_lock = Mutex.create ()

let get_access_token ?(scopes : string list = []) () :
    (token_info, [> error ]) result =
  let get_new_access_token scopes =
    let open CCResult.Infix in
    let* credentials = discover_credentials () in
    let+ access_token = access_token_of_credentials scopes credentials in
    Log.info (fun m -> m "Authenticated OK!");
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
  Mutex.protect token_lock (fun () ->
      let result =
        match !token_cache with
        | Some token_info
          when has_requested_scopes token_info && not (is_expired token_info) ->
            Ok token_info
        | Some token_info ->
            Log.debug (fun m ->
                if is_expired token_info then
                  m "Re-authenticating: Token is expired"
                else m "Re-authenticating: Token does not have required scopes");
            get_new_access_token
              (CCList.union ~eq:String.equal token_info.scopes scopes)
        | None -> get_new_access_token scopes
      in
      token_cache := CCResult.to_opt result;
      result)
