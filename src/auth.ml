(* https://developers.google.com/identity/protocols/OAuth2#serviceaccount *)
(* https://github.com/GoogleCloudPlatform/google-auth-library-python/blob/7e1270b1e5a99171fee4abfef6a4b9217ed378d7/google/auth/_default.py#L186 *)

let src = Logs.Src.create "gcloud.auth"

module L = (val Logs_lwt.src_log src)

let ok = Lwt_result.ok

module Environment_vars = struct
  let google_application_credentials = "GOOGLE_APPLICATION_CREDENTIALS"

  let google_application_credentials_json =
    "GOOGLE_APPLICATION_CREDENTIALS_JSON"

  let google_project_id = "GOOGLE_PROJECT_ID"
  let gce_metadata_ip = "GCE_METADATA_IP"
  let gce_metadata_root = "GCE_METADATA_ROOT"
  let gce_metadata_timeout = "GCE_METADATA_TIMEOUT"
end

module Paths = struct
  let application_default_credentials =
    String.concat "/"
      [
        Sys.getenv "HOME"; ".config/gcloud/application_default_credentials.json";
      ]

  let active_config =
    String.concat "/" [ Sys.getenv "HOME"; ".config/gcloud/active_config" ]

  let config ~active_config_name =
    String.concat "/"
      [
        Sys.getenv "HOME";
        ".config/gcloud/configurations";
        Format.asprintf "config_%s" active_config_name;
      ]
end

module Scopes = struct
  let iam = "https://www.googleapis.com/auth/iam"
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
end

module Compute_engine = struct
  module Metadata = struct
    type error = [ `Bad_GCE_metadata_response of Cohttp.Code.status_code ]

    let pp_error fmt (error : error) =
      match error with
      | `Bad_GCE_metadata_response status_code ->
          Format.fprintf fmt
            "GCE metadata API returned unexpected response code: %s"
            (Cohttp.Code.string_of_status status_code)

    let metadata_ip_root =
      let metadata_ip =
        Sys.getenv_opt Environment_vars.gce_metadata_ip
        |> CCOption.get_or ~default:"169.254.169.254"
      in
      Printf.sprintf "http://%s" metadata_ip

    let metadata_root =
      let host =
        Sys.getenv_opt Environment_vars.gce_metadata_root
        |> CCOption.get_or ~default:"metadata.google.internal"
      in
      Printf.sprintf "http://%s/computeMetadata/v1" host

    let metadata_flavor_header = "metadata-flavor"
    let metadata_flavor_value = "Google"

    let metadata_headers =
      Cohttp.Header.of_list [ (metadata_flavor_header, metadata_flavor_value) ]

    let metadata_default_timeout =
      let default = 3. in
      Sys.getenv_opt Environment_vars.gce_metadata_timeout
      |> CCOption.map (fun str ->
             try float_of_string str with Failure _ -> default)
      |> CCOption.get_or ~default

    let response_has_metadata_header (response : Cohttp.Response.t) =
      Cohttp.Header.get
        (Cohttp.Response.headers response)
        metadata_flavor_header
      = Some metadata_flavor_value

    let ping () =
      let uri = Uri.of_string metadata_ip_root in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers
      >>= Util.drain_body

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
end

type error =
  [ `Bad_token_response of string
  | `Bad_credentials_format
  | `Bad_credentials_priv_key of string
  | `Jwt_signing_error of string
  | `No_credentials
  | `Bad_subject_token_response of Cohttp.Response.t * string
  | Compute_engine.Metadata.error ]

let pp_error fmt (error : error) =
  match error with
  | `Bad_token_response body_str ->
      Format.fprintf fmt "Unexpected format for access_token: %S" body_str
  | `Bad_credentials_format ->
      Format.fprintf fmt "Unexpected format for credentials"
  | `Bad_credentials_priv_key msg ->
      Format.fprintf fmt "Could not decode private key from credentials: %s" msg
  | `Jwt_signing_error msg -> Format.fprintf fmt "Could not sign JWT: %s" msg
  | `No_credentials -> Format.fprintf fmt "Could not discover credentials"
  | `Bad_subject_token_response (res, body_str) ->
      let status = Cohttp.Response.status res in
      let status_str = Cohttp.Code.string_of_status status in
      Format.fprintf fmt
        "Unexpected response (%s) while fetching subject token: %s" status_str
        body_str
  | #Compute_engine.Metadata.error as e ->
      Compute_engine.Metadata.pp_error fmt e

exception Error of error

type user_refresh_credentials = {
  client_id : string;
  client_secret : string;
  refresh_token : string;
}

type service_account_credentials = {
  client_email : string;
  private_key : string;
  project_id : string;
  token_uri : string;
}

module External_account_credentials = struct
  type headers = { authorization : string }

  let headers_of_json (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let authorization = json |> member "Authorization" |> to_string in
    { authorization }

  type format = { type_ : [ `Json ]; subject_token_field_name : string }

  let format_of_json (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let type_ = json |> member "type" |> to_string in
    let subject_token_field_name =
      json |> member "subject_token_field_name" |> to_string
    in
    {
      type_ =
        (if type_ = "json" then `Json
        else
          raise
            (Type_error
               ( Format.asprintf "Unknown credential_source.format.type: %s"
                   type_,
                 json )));
      subject_token_field_name;
    }

  type credential_source = {
    url : string;
    headers : (string * string) list;
    format : format;
  }

  let credential_source_of_json (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let url = json |> member "url" |> to_string in
    let headers =
      json |> member "headers" |> to_assoc
      |> CCList.map (fun (k, v) -> (k, to_string v))
    in
    let format = json |> member "format" |> format_of_json in
    { url; headers; format }

  type t = {
    audience : string;
    subject_token_type : string;
    token_url : string;  (** token exchange endpoint *)
    service_account_impersonation_url : string option;
        (** URL of gcloud endpoint to perform service-account impersonation, once authed via token exchange *)
    credential_source : credential_source;
        (** Details of how to fetch an initial subject token, to be exchanged for a gcloud token via the endpoint at [token_url] *)
  }

  let of_json (json : Yojson.Basic.t) : t =
    let open Yojson.Basic.Util in
    let audience = json |> member "audience" |> to_string in
    let subject_token_type = json |> member "subject_token_type" |> to_string in
    let token_url = json |> member "token_url" |> to_string in
    let service_account_impersonation_url =
      json |> member "service_account_impersonation_url" |> to_option to_string
    in
    let credential_source =
      json |> member "credential_source" |> credential_source_of_json
    in
    {
      audience;
      subject_token_type;
      token_url;
      service_account_impersonation_url;
      credential_source;
    }

  let subject_token_of_json (t : t) (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    json
    |> member t.credential_source.format.subject_token_field_name
    |> to_string

  let subject_token_of_response (t : t)
      ((resp, body_str) : Cohttp.Response.t * string) :
      (string, [> `Bad_token_response of string ]) result Lwt.t =
    let open Lwt.Syntax in
    match Cohttp.Response.status resp with
    | `OK -> (
        match t.credential_source.format.type_ with
        | `Json -> (
            try
              body_str |> Yojson.Basic.from_string |> subject_token_of_json t
              |> Lwt.return_ok
            with Yojson.Basic.Util.Type_error (msg, _) ->
              let* () = L.debug (fun m -> m "Type_error: %s" msg) in
              Lwt.return_error (`Bad_subject_token_response (resp, body_str))))
    | _ ->
        let* () = L.err (fun m -> m "response: %s" body_str) in
        Lwt.return_error (`Bad_subject_token_response (resp, body_str))
end

type gce_metadata_details = { project_id : string }

type credentials =
  | Authorized_user of user_refresh_credentials
  | Service_account of service_account_credentials
  | External_account of External_account_credentials.t
  | GCE_metadata of gce_metadata_details

(* On Google Compute Engine, we don't need credentials *)

module Access_token = struct
  type t = {
    access_token : string;
    expires_in : int;
    additional_refresh_scopes : string list;
  }

  let make ~access_token ~expires_in ?(additional_refresh_scopes = []) () =
    { access_token; expires_in; additional_refresh_scopes }
end

type token_info = {
  credentials : credentials;
  token : Access_token.t;
  created_at : float;
  scopes : string list;
}

let token_info_mvar : token_info option Lwt_mvar.t = Lwt_mvar.create None

let access_token_of_json (json : Yojson.Basic.t) :
    (Access_token.t, [> `Bad_token_response of string ]) result =
  let open Yojson.Basic.Util in
  try
    let access_token = json |> member "access_token" |> to_string in
    let expires_in = json |> member "expires_in" |> to_int in
    Ok (Access_token.make ~access_token ~expires_in ())
  with Yojson.Basic.Util.Type_error (_msg, _) ->
    Error (`Bad_token_response Yojson.Basic.(to_string json))

let authorized_user_credentials_of_json (json : Yojson.Basic.t) :
    user_refresh_credentials =
  let open Yojson.Basic.Util in
  let client_id = json |> member "client_id" |> to_string in
  let client_secret = json |> member "client_secret" |> to_string in
  let refresh_token = json |> member "refresh_token" |> to_string in
  { client_id; client_secret; refresh_token }

let service_account_credentials_of_json (json : Yojson.Basic.t) :
    service_account_credentials =
  let open Yojson.Basic.Util in
  let client_email = json |> member "client_email" |> to_string in
  let private_key = json |> member "private_key" |> to_string in
  let project_id = json |> member "project_id" |> to_string in
  let token_uri = json |> member "token_uri" |> to_string in
  { client_email; private_key; project_id; token_uri }

let credentials_of_json (json : Yojson.Basic.t) : credentials =
  let open Yojson.Basic.Util in
  let cred_type = json |> member "type" |> to_string in
  match cred_type with
  | "authorized_user" ->
      Authorized_user (authorized_user_credentials_of_json json)
  | "service_account" ->
      Service_account (service_account_credentials_of_json json)
  | "external_account" ->
      (* https://github.com/googleapis/google-auth-library-python/blob/9c87ad07c6618bc5b1be3b254fdf5211e7778061/google/oauth2/sts.py#L141 *)
      (* https://cloud.google.com/iam/docs/reference/sts/rest/v1/TopLevel/token *)
      (* https://google.aip.dev/auth/4117 *)
      External_account (External_account_credentials.of_json json)
  | _ ->
      raise
        (Type_error
           (Printf.sprintf "Unknown credentials type: %S" cred_type, json))

type discovery_mode =
  | Discover_credentials_path_from_env
  | Discover_credentials_json_from_env
  | Discover_credentials_from_cloud_sdk_path
  | Discover_credentials_from_gce_metadata

let pp_discovery_mode : discovery_mode CCFormat.printer =
 fun fmt discovery_mode ->
  match discovery_mode with
  | Discover_credentials_path_from_env ->
      Format.fprintf fmt "Discover_credentials_path_from_env"
  | Discover_credentials_json_from_env ->
      Format.fprintf fmt "Discover_credentials_json_from_env"
  | Discover_credentials_from_cloud_sdk_path ->
      Format.fprintf fmt "Discover_credentials_from_cloud_sdk_path"
  | Discover_credentials_from_gce_metadata ->
      Format.fprintf fmt "Discover_credentials_from_gce_metadata"

let credentials_of_string (json_str : string) :
    (credentials, [> `Bad_credentials_format ]) result =
  try
    json_str |> Yojson.Basic.from_string |> credentials_of_json |> CCResult.pure
  with Yojson.Basic.Util.Type_error (_msg, _) ->
    CCResult.fail `Bad_credentials_format

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

let access_token_of_response ?(of_json = access_token_of_json)
    ((resp, body_str) : Cohttp.Response.t * string) :
    (Access_token.t, [> `Bad_token_response of string ]) result Lwt.t =
  let open Lwt.Syntax in
  match Cohttp.Response.status resp with
  | `OK -> body_str |> Yojson.Basic.from_string |> of_json |> Lwt.return
  | _ ->
      let* () = L.err (fun m -> m "response: %s" body_str) in
      Lwt.return_error (`Bad_token_response body_str)

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
      access_token_of_response ~of_json:access_token_of_json res
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
      access_token_of_response ~of_json:access_token_of_json res
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
      | None -> access_token_of_response ~of_json:access_token_of_json res
      | Some sac ->
          let* () =
            L.debug (fun m -> m "attempting to impersonate service account")
            |> ok
          in
          let* initial_access_token = access_token_of_response res in
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
          let access_token_of_json (json : Yojson.Basic.t) :
              (Access_token.t, [> error ]) result =
            (* has a slightly different format from the access token in the other responses:
               - camel case fields
               - expireTime rather than expiresIn
            *)
            let open CCResult.Infix in
            let* access_token, expire_time =
              try
                let open Yojson.Basic.Util in
                let access_token = json |> member "accessToken" |> to_string in
                let expire_time = json |> member "expireTime" |> to_string in
                Ok (access_token, expire_time)
              with Yojson.Basic.Util.Type_error (_msg, _) ->
                Error (`Bad_token_response Yojson.Basic.(to_string json))
            in
            let* t, _tz, _count =
              Ptime.of_rfc3339 expire_time
              |> CCResult.map_err (fun _ ->
                     `Bad_token_response
                       (Format.asprintf "couldn't parse expireTime from: %s"
                          Yojson.Basic.(to_string json)))
            in
            let now = Ptime_clock.now () in
            let* expires_in =
              match Ptime.diff t now |> Ptime.Span.to_int_s with
              | None ->
                  Error (`Bad_token_response Yojson.Basic.(to_string json))
              | Some expires_in -> Ok expires_in
            in
            Ok
              (Access_token.make ~access_token ~expires_in
                 ~additional_refresh_scopes:[ Scopes.iam ] ())
          in
          access_token_of_response ~of_json:access_token_of_json res)

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
            let* resp = Compute_engine.Metadata.ping () |> ok in
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
                let* project_id = Compute_engine.Metadata.get_project_id () in
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
