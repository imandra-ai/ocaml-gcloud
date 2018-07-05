(* https://developers.google.com/identity/protocols/OAuth2#serviceaccount *)
(* https://github.com/GoogleCloudPlatform/google-auth-library-python/blob/7e1270b1e5a99171fee4abfef6a4b9217ed378d7/google/auth/_default.py#L186 *)

let src =
  Logs.Src.create "gcloud.auth"

module L = (val Logs_lwt.src_log src)

module Environment_vars = struct
  let google_application_credentials = "GOOGLE_APPLICATION_CREDENTIALS"
  let google_application_credentials_json = "GOOGLE_APPLICATION_CREDENTIALS_JSON"

  let google_project_id = "GOOGLE_PROJECT_ID"

  let gce_metadata_ip = "GCE_METADATA_IP"
  let gce_metadata_root = "GCE_METADATA_ROOT"
  let gce_metadata_timeout = "GCE_METADATA_TIMEOUT"
end

module Paths = struct
  let application_default_credentials =
    String.concat "/"
      [ Sys.getenv "HOME"; ".config/gcloud/application_default_credentials.json" ]
end

module Compute_engine = struct
  module Metadata = struct
    type error =
      [ `Bad_GCE_metadata_response of Cohttp.Code.status_code ]

    let metadata_ip_root =
      let metadata_ip =
        Sys.getenv_opt Environment_vars.gce_metadata_ip
        |> CCOpt.get_or ~default:"169.254.169.254"
      in
      Printf.sprintf "http://%s" metadata_ip

    let metadata_root =
      let host =
        Sys.getenv_opt Environment_vars.gce_metadata_root
        |> CCOpt.get_or ~default:"metadata.google.internal"
      in
      Printf.sprintf "http://%s/computeMetadata/v1" host

    let metadata_flavor_header = "metadata-flavor"
    let metadata_flavor_value = "Google"
    let metadata_headers = Cohttp.Header.of_list [(metadata_flavor_header, metadata_flavor_value)]

    let metadata_default_timeout =
      let default = 3. in
      Sys.getenv_opt Environment_vars.gce_metadata_timeout
      |> CCOpt.map (fun str -> try float_of_string str with | Failure _ -> default)
      |> CCOpt.get_or ~default

    let response_has_metadata_header (response : Cohttp.Response.t) =
      Cohttp.Header.get (Cohttp.Response.headers response) metadata_flavor_header = Some metadata_flavor_value

    let ping () =
      let uri = Uri.of_string metadata_ip_root in
      Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers

    let get_project_id () : (string, [> `Bad_GCE_metadata_response of Cohttp.Code.status_code ]) Lwt_result.t =
      let open Lwt.Infix in
      let uri = Uri.of_string (Printf.sprintf "%s/project/project-id" metadata_root) in
      Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers >>= fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `OK -> Cohttp_lwt.Body.to_string body |> Lwt_result.ok
      | status -> `Bad_GCE_metadata_response status |> Lwt_result.fail
  end
end

module Cloud_sdk = struct
  let get_project_id () =
    Lwt_process.with_process_in ("gcloud", [| "gcloud"; "config"; "get-value"; "core/project" |])
      (fun process_in ->
         let open Lwt.Infix in
         process_in#status >>= fun status ->
         Lwt_io.read process_in#stdout >|= String.trim)
end

type error =
  [ `Bad_token_response
  | `Bad_credentials_format
  | `No_credentials
  | Compute_engine.Metadata.error
  ]

exception Error of error

type user_refresh_credentials =
  { client_id : string
  ; client_secret : string
  ; refresh_token : string
  }

type service_account_credentials =
  { client_email : string
  ; private_key : string
  ; project_id : string
  ; token_uri : string
  }

type credentials =
  | Authorized_user of user_refresh_credentials
  | Service_account of service_account_credentials
  | GCE_metadata (* On Google Compute Engine, we don't need credentials *)

type access_token =
  { access_token : string
  ; expires_in : int
  }

type token_info =
  { credentials : credentials
  ; token : access_token
  ; created_at : float
  ; scopes : string list
  ; project_id : string option
  }

let token_info_mvar : token_info option Lwt_mvar.t =
  Lwt_mvar.create None

let access_token_of_json (json : Yojson.Basic.json) : access_token =
  let open Yojson.Basic.Util in
  let access_token = json |> member "access_token" |> to_string in
  let expires_in = json |> member "expires_in" |> to_int in
  { access_token; expires_in }

let authorized_user_credentials_of_json (json : Yojson.Basic.json) : user_refresh_credentials =
  let open Yojson.Basic.Util in
  let client_id = json |> member "client_id" |> to_string in
  let client_secret = json |> member "client_secret" |> to_string in
  let refresh_token = json |> member "refresh_token" |> to_string in
  { client_id; client_secret; refresh_token }

let service_account_credentials_of_json (json : Yojson.Basic.json) : service_account_credentials =
  let open Yojson.Basic.Util in
  let client_email = json |> member "client_email" |> to_string in
  let private_key = json |> member "private_key" |> to_string in
  let project_id = json |> member "project_id" |> to_string in
  let token_uri = json |> member "token_uri" |> to_string in
  { client_email; private_key; project_id; token_uri }

let credentials_of_json (json : Yojson.Basic.json) : credentials =
  let open Yojson.Basic.Util in
  let cred_type = json |> member "type" |> to_string in
  match cred_type with
  | "authorized_user" -> Authorized_user (authorized_user_credentials_of_json json)
  | "service_account" -> Service_account (service_account_credentials_of_json json)
  | _ -> raise (Type_error ((Printf.sprintf "Unknown credentials type: %S" cred_type), json))

type discovery_mode =
  | Discover_credentials_path_from_env
  | Discover_credentials_json_from_env
  | Discover_credentials_from_cloud_sdk_path
  | Discover_credentials_from_gce_metadata

let pp_discovery_mode : discovery_mode CCFormat.printer =
  fun fmt discovery_mode ->
    match discovery_mode with
    | Discover_credentials_path_from_env -> Format.fprintf fmt "Discover_credentials_path_from_env"
    | Discover_credentials_json_from_env -> Format.fprintf fmt "Discover_credentials_json_from_env"
    | Discover_credentials_from_cloud_sdk_path -> Format.fprintf fmt "Discover_credentials_from_cloud_sdk_path"
    | Discover_credentials_from_gce_metadata -> Format.fprintf fmt "Discover_credentials_from_gce_metadata"

let credentials_of_string (json_str : string)
  : (credentials, [> `Bad_credentials_format ]) result =
  try
    json_str
    |> Yojson.Basic.from_string
    |> credentials_of_json
    |> CCResult.pure
  with
  | Yojson.Basic.Util.Type_error (msg, _) ->
    CCResult.fail `Bad_credentials_format

let credentials_of_file (credentials_file : string)
  : (credentials, [> `No_credentials | `Bad_credentials_format]) result Lwt.t =
  let open Lwt.Infix in
  Lwt_unix.file_exists credentials_file >>= fun exists ->
  if not exists then
    Lwt.return_error `No_credentials
  else
    Lwt_io.(with_file ~mode:input) credentials_file
      (fun input_chan ->
         Lwt_io.read_lines input_chan |> Lwt_stream.to_list >>= fun lines ->
         lines
         |> String.concat "\n"
         |> credentials_of_string
         |> Lwt.return
      )

let access_token_of_response (resp, body : Cohttp.Response.t * Cohttp_lwt.Body.t) : (access_token, [>`Bad_token_response]) result Lwt.t =
  let open Lwt.Infix in
  match Cohttp.Response.status resp with
  | `OK -> begin
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      try
        body_str
        |> Yojson.Basic.from_string
        |> access_token_of_json
        |> Lwt.return_ok
      with
      | Yojson.Basic.Util.Type_error (msg, _) ->
        Lwt.return_error `Bad_token_response
    end
  | _ ->
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    L.err (fun m -> m "response: %s" body_str) >>= fun () ->
    Lwt.return_error `Bad_token_response

let access_token_of_credentials (scopes : string list) (credentials : credentials)
  : (access_token, [> `Bad_token_response ]) result Lwt.t =
  let open Lwt.Infix in
  let request =
    match credentials with
    | Authorized_user c ->
      let token_uri = Uri.make ()
          ~scheme:"https"
          ~host:"www.googleapis.com"
          ~path:"oauth2/v4/token"
      in
      let params =
        [ ( "client_id", [ c.client_id ] )
        ; ( "client_secret", [ c.client_secret ] )
        ; ( "refresh_token", [ c.refresh_token ] )
        ; ( "grant_type", [ "refresh_token" ] )
        ]
      in
      Cohttp_lwt_unix.Client.post_form token_uri ~params
    | Service_account c ->
      let key =
        Cstruct.of_string c.private_key
        |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
      in
      Nocrypto_entropy_lwt.initialize () >>= fun () ->
      let now = Unix.time () in
      let header = Jwt.(header_of_algorithm_and_typ (RS256 (match key with | `RSA k -> k)) (Some "JWT")) in
      let payload =
        Jwt.empty_payload
        |> Jwt.add_claim Jwt.iss c.client_email
        |> Jwt.add_claim (Jwt.claim "scope") (String.concat " " scopes)
        |> Jwt.add_claim Jwt.aud c.token_uri
        |> Jwt.add_claim Jwt.iat (Printf.sprintf "%.0f" now)
        |> Jwt.add_claim Jwt.exp (Printf.sprintf "%.0f" (now +. 3600.))
      in
      let jwt = Jwt.t_of_header_and_payload header payload in
      let token = Jwt.token_of_t jwt in
      let params =
        [ ( "grant_type", [ "urn:ietf:params:oauth:grant-type:jwt-bearer" ] )
        ; ( "assertion", [ token ] )
        ]
      in
      Cohttp_lwt_unix.Client.post_form (Uri.of_string c.token_uri) ~params
    | GCE_metadata ->
      let uri =
        Printf.sprintf "%s/instance/service-accounts/default/token" Compute_engine.Metadata.metadata_root
        |> Uri.of_string
      in
      Cohttp_lwt_unix.Client.get uri ~headers:Compute_engine.Metadata.metadata_headers
  in
  request >>= access_token_of_response

let project_id_of_credentials (credentials : credentials) : string option =
  match credentials with
  | Service_account { project_id } -> Some project_id
  | Authorized_user _
  | GCE_metadata -> None

let discover_project_id (credentials : credentials) : string option =
  CCOpt.choice
    [ Sys.getenv_opt Environment_vars.google_project_id
    ; project_id_of_credentials credentials
    ]

let discover_credentials_with (discovery_mode : discovery_mode) =
  let open Lwt.Infix in
  L.debug (fun m -> m "Attempting authentication using %a" pp_discovery_mode discovery_mode) >>= fun () ->
  match discovery_mode with
  | Discover_credentials_path_from_env -> begin
      let credentials_file = Sys.getenv_opt Environment_vars.google_application_credentials in
      match credentials_file with
      | None -> Lwt.return_error `No_credentials
      | Some credentials_file ->
        let open Lwt_result.Infix in
        credentials_of_file credentials_file >>= fun credentials ->
        Lwt_result.return (credentials, discover_project_id credentials)
    end

  | Discover_credentials_json_from_env ->
    let credentials_json = Sys.getenv_opt Environment_vars.google_application_credentials_json in
    begin match credentials_json with
      | None -> Lwt.return_error `No_credentials
      | Some json_str ->
        let open Lwt_result.Infix in
        credentials_of_string json_str |> Lwt.return >>= fun credentials ->
        Lwt_result.return (credentials, discover_project_id credentials)
    end

  | Discover_credentials_from_cloud_sdk_path ->
    let open Lwt_result.Infix in
    credentials_of_file Paths.application_default_credentials >>= fun credentials ->
    begin match discover_project_id credentials with
    | Some project_id -> Lwt_result.return (credentials, Some project_id)
    | None ->
      Cloud_sdk.get_project_id () |> Lwt_result.ok >>= fun project_id ->
      Lwt_result.return (credentials, Some project_id)
    end

  | Discover_credentials_from_gce_metadata ->
    let ping =
      Lwt.catch
        (fun () ->
           Compute_engine.Metadata.ping () >>= fun (resp, body) ->
           match Cohttp.Response.status resp with
           | `OK when Compute_engine.Metadata.response_has_metadata_header resp ->
             let open Lwt_result.Infix in
             let credentials = GCE_metadata in
             let project_id = discover_project_id credentials in
             (match project_id with
              | Some project_id -> Lwt_result.return project_id
              | None -> Compute_engine.Metadata.get_project_id ()
             ) >>= fun project_id ->
             Lwt.return_ok (credentials, Some project_id)
           | _ ->
             Lwt.return_error `No_credentials
        )
        (fun exn -> Lwt.return_error `No_credentials)
    in
    let timeout =
      Lwt_unix.sleep Compute_engine.Metadata.metadata_default_timeout >>= fun () ->
      Lwt.return_error `No_credentials
    in
    Lwt.pick [ ping; timeout ]

let rec first_ok ~(error : 'e) (fs : (unit -> ('a, 'e) result Lwt.t) list) : ('a, 'e) result Lwt.t =
  match fs with
  | [] -> Lwt.return_error error
  | t :: fs -> begin
      let open Lwt.Infix in
      t () >>= function
      | Ok x -> Lwt.return_ok x
      | Error error -> first_ok ~error fs
    end

let discover_credentials () : (credentials * string option, error) Lwt_result.t =
  let open Lwt.Infix in
  [ Discover_credentials_path_from_env
  ; Discover_credentials_json_from_env
  ; Discover_credentials_from_cloud_sdk_path
  ; Discover_credentials_from_gce_metadata
  ]
  |> List.map (fun discovery_mode -> fun () -> discover_credentials_with discovery_mode)
  |> first_ok ~error:`No_credentials

let get_access_token ?(scopes : string list = []) () : token_info Lwt.t =
  let get_new_access_token scopes =
    let open Lwt_result.Infix in
    discover_credentials () >>= fun (credentials, project_id) ->
    access_token_of_credentials scopes credentials >>= fun access_token ->
    L.info (fun m ->
        m "Authenticated OK (project: %s)"
          (project_id |> CCOpt.get_or ~default:"no project"))
    |> Lwt_result.ok >|= fun () ->
    { credentials
    ; token = access_token
    ; created_at = Unix.time ()
    ; scopes
    ; project_id
    }
  in
  let has_requested_scopes token_info = CCList.subset ~eq:String.equal scopes token_info.scopes in
  let is_expired token_info = Unix.time () > token_info.created_at +. (float_of_int token_info.token.expires_in) in
  let open Lwt.Infix in
  Lwt_mvar.take token_info_mvar >>= begin function
    | Some token_info when has_requested_scopes token_info && not (is_expired token_info) ->
      Lwt.return_ok token_info
    | Some token_info ->
      begin if is_expired token_info then
          L.debug (fun m -> m "Re-authenticating: Token is expired")
        else
          L.debug (fun m -> m "Re-authenticating: Token does not have required scopes")
      end >>= fun () ->
      get_new_access_token (CCList.union ~eq:String.equal token_info.scopes scopes)
    | None -> get_new_access_token scopes
  end >>= fun token_info_result ->
  Lwt_mvar.put token_info_mvar (CCResult.to_opt token_info_result) >>= fun () ->
  match token_info_result with
  | Ok token_info -> Lwt.return token_info
  | Error error -> Lwt.fail (Error error)

let get_project_id (scopes : string list) =
  let open Lwt.Infix in
  get_access_token ~scopes () >>= fun token_info ->
  Lwt.return token_info.project_id
