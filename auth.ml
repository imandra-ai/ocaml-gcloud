(* https://developers.google.com/identity/protocols/OAuth2#serviceaccount *)
let section = Lwt_log.Section.make "gcloud.auth"

type error =
  [ `Bad_token_response
  | `Bad_credentials_format
  | `No_credentials
  ]

exception Error of error

type access_token =
  { access_token : string
  ; expires_in : int
  }

type token_info =
  { token : access_token
  ; created_at : float
  ; scopes : string list
  }

let token_info_mvar : token_info option Lwt_mvar.t =
  Lwt_mvar.create None

let access_token_of_json (json : Yojson.Basic.json) : access_token =
  let open Yojson.Basic.Util in
  let access_token = json |> member "access_token" |> to_string in
  let expires_in = json |> member "expires_in" |> to_int in
  { access_token; expires_in }

type user_refresh_credentials =
  { client_id : string
  ; client_secret : string
  ; refresh_token : string
  }

type service_account_credentials =
  { client_email : string
  ; private_key : string
  }

type credentials =
  | Authorized_user of user_refresh_credentials
  | Service_account of service_account_credentials

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
  { client_email; private_key }

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
  | _ -> Lwt.return_error `Bad_token_response

let token_request_of_credentials (scopes : string list) (credentials : credentials)
  : (access_token, [> `Bad_token_response ]) result Lwt.t =
  let open Lwt.Infix in
  let token_uri = Uri.make ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:"oauth2/v4/token"
  in
  let params =
    match credentials with
    | Authorized_user c ->
      Lwt.return
        [ ( "client_id", [ c.client_id ] )
        ; ( "client_secret", [ c.client_secret ] )
        ; ( "refresh_token", [ c.refresh_token ] )
        ; ( "grant_type", [ "refresh_token" ] )
        ]
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
        |> Jwt.add_claim Jwt.aud (Uri.to_string token_uri)
        |> Jwt.add_claim Jwt.iat (Printf.sprintf "%.0f" now)
        |> Jwt.add_claim Jwt.exp (Printf.sprintf "%.0f" (now +. 3600.))
      in
      Lwt.wrap2 Jwt.t_of_header_and_payload header payload >>= fun jwt ->
      let token = Jwt.token_of_t jwt in
      Lwt.return
        [ ( "grant_type", [ "urn:ietf:params:oauth:grant-type:jwt-bearer" ] )
        ; ( "assertion", [ token ] )
        ]
  in
  params >>= fun params ->
  Cohttp_lwt_unix.Client.post_form token_uri ~params >>=
  access_token_of_response

let discover_credentials_with ?(scopes = []) (discovery_mode : discovery_mode) =
  let open Lwt.Infix in
  Lwt_log.debug_f ~section "Attempting authentication using %s" (CCFormat.to_string pp_discovery_mode discovery_mode) >>= fun () ->
  match discovery_mode with
  | Discover_credentials_path_from_env -> begin
      let credentials_file =
        try
          Some (Sys.getenv "GOOGLE_APPLICATION_CREDENTIALS")
        with
        | Not_found -> None
      in
      match credentials_file with
      | None -> Lwt.return_error `No_credentials
      | Some credentials_file ->
        let open Lwt_result.Infix in
        credentials_of_file credentials_file >>= fun credentials ->
        token_request_of_credentials scopes credentials
    end

  | Discover_credentials_json_from_env ->
    let credentials_json =
      try
        Some (Sys.getenv "GOOGLE_APPLICATION_CREDENTIALS_JSON")
      with
      | Not_found -> None
    in
    let open Lwt_result.Infix in
    begin match credentials_json with
      | None -> Lwt.return_error `No_credentials
      | Some json_str ->
        credentials_of_string json_str
        |> Lwt.return
    end >>= fun credentials ->
    token_request_of_credentials scopes credentials

  | Discover_credentials_from_cloud_sdk_path ->
    let credentials_file =
      String.concat "/"
        [ Sys.getenv "HOME"; ".config/gcloud/application_default_credentials.json" ]
    in
    let open Lwt_result.Infix in
    credentials_of_file credentials_file >>= fun credentials ->
    token_request_of_credentials scopes credentials

  | Discover_credentials_from_gce_metadata ->
    let open Lwt.Infix in
    let uri =
      [ "http://metadata.google.internal/computeMetadata/v1/"
      ; "instance/service-accounts/default/token"
      ]
      |> String.concat ""
      |> Uri.of_string
    in
    Lwt.catch
      (fun () ->
         Cohttp_lwt_unix.Client.get uri
           ~headers:(Cohttp.Header.of_list [("Metadata-Flavor", "Google")])
         >>= access_token_of_response)
      (fun exn -> Lwt.return_error `No_credentials)

let rec first_ok ~(error : 'e) (fs : (unit -> ('a, 'e) result Lwt.t) list) : ('a, 'e) result Lwt.t =
  match fs with
  | [] -> Lwt.return_error error
  | t :: fs -> begin
      let open Lwt.Infix in
      t () >>= function
      | Ok x -> Lwt.return_ok x
      | Error error -> first_ok ~error fs
    end

let discover_credentials (scopes : string list) : (access_token, error) Lwt_result.t =
  let open Lwt.Infix in
  [ Discover_credentials_path_from_env
  ; Discover_credentials_json_from_env
  ; Discover_credentials_from_cloud_sdk_path
  ; Discover_credentials_from_gce_metadata
  ]
  |> List.map (fun discovery_mode ->
      fun () -> discover_credentials_with ~scopes discovery_mode
    )
  |> first_ok ~error:`No_credentials

let get_access_token ?(scopes : string list = []) () : token_info Lwt.t =
  let get_new_access_token scopes =
    let open Lwt_result.Infix in
    discover_credentials scopes >>= fun access_token ->
    Lwt_log.debug_f ~section "Authenticated OK" |> Lwt_result.ok >|= fun () ->
    { token = access_token
    ; created_at = Unix.time ()
    ; scopes
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
          Lwt_log.debug ~section "Re-authenticating: Token is expired"
        else
          Lwt_log.debug ~section "Re-authenticating: Token does not have required scopes"
      end >>= fun () ->
      get_new_access_token (CCList.union ~eq:String.equal token_info.scopes scopes)
    | None -> get_new_access_token scopes
  end >>= fun token_info_result ->
  Lwt_mvar.put token_info_mvar (CCResult.to_opt token_info_result) >>= fun () ->
  match token_info_result with
  | Ok token_info -> Lwt.return token_info
  | Error error -> Lwt.fail (Error error)
