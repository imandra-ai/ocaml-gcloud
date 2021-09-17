(* https://developers.google.com/identity/protocols/OAuth2#serviceaccount *)
(* https://github.com/GoogleCloudPlatform/google-auth-library-python/blob/7e1270b1e5a99171fee4abfef6a4b9217ed378d7/google/auth/_default.py#L186 *)

let src = Logs.Src.create "gcloud.auth"

module L = (val Logs_lwt.src_log src)

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
    String.concat
      "/"
      [ Sys.getenv "HOME"
      ; ".config/gcloud/application_default_credentials.json"
      ]
end

module Compute_engine = struct
  module Metadata = struct
    type error = [ `Bad_GCE_metadata_response of Cohttp.Code.status_code ]

    let pp_error fmt (error : error) =
      match error with
      | `Bad_GCE_metadata_response status_code ->
          Format.fprintf
            fmt
            "GCE metadata API returned unexpected response code: %s"
            (Cohttp.Code.string_of_status status_code)


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

    let metadata_headers =
      Cohttp.Header.of_list [ (metadata_flavor_header, metadata_flavor_value) ]


    let metadata_default_timeout =
      let default = 3. in
      Sys.getenv_opt Environment_vars.gce_metadata_timeout
      |> CCOpt.map (fun str ->
             try float_of_string str with Failure _ -> default )
      |> CCOpt.get_or ~default


    let response_has_metadata_header (response : Cohttp.Response.t) =
      Cohttp.Header.get
        (Cohttp.Response.headers response)
        metadata_flavor_header
      = Some metadata_flavor_value


    let ping () =
      let uri = Uri.of_string metadata_ip_root in
      Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers


    let get_project_id () :
        ( string
        , [> `Bad_GCE_metadata_response of Cohttp.Code.status_code ] )
        Lwt_result.t =
      let open Lwt.Infix in
      let uri =
        Uri.of_string (Printf.sprintf "%s/project/project-id" metadata_root)
      in
      Cohttp_lwt_unix.Client.get uri ~headers:metadata_headers
      >>= fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `OK ->
          Cohttp_lwt.Body.to_string body |> Lwt_result.ok
      | status ->
          `Bad_GCE_metadata_response status |> Lwt_result.fail
  end
end

module Cloud_sdk = struct
  let get_project_id () =
    Lwt_process.with_process_in
      ("gcloud", [| "gcloud"; "config"; "get-value"; "core/project" |])
      (fun process_in ->
        let open Lwt.Infix in
        process_in#status
        >>= fun _status -> Lwt_io.read process_in#stdout >|= String.trim )
end

type error =
  [ `Bad_token_response of string
  | `Bad_credentials_format
  | `Bad_credentials_priv_key of string
  | `Jwt_signing_error of string
  | `No_credentials
  | `No_project_id
  | Compute_engine.Metadata.error
  ]

let pp_error fmt (error : error) =
  match error with
  | `Bad_token_response body_str ->
      Format.fprintf fmt "Unexpected format for access_token: %S" body_str
  | `Bad_credentials_format ->
      Format.fprintf fmt "Unexpected format for credentials"
  | `Bad_credentials_priv_key msg ->
      Format.fprintf fmt "Could not decode private key from credentials: %s" msg
  | `Jwt_signing_error msg ->
      Format.fprintf fmt "Could not sign JWT: %s" msg
  | `No_credentials ->
      Format.fprintf fmt "Could not discover credentials"
  | `No_project_id ->
      Format.fprintf
        fmt
        "Could not discover the project ID (try setting %s)"
        Environment_vars.google_project_id
  | #Compute_engine.Metadata.error as e ->
      Compute_engine.Metadata.pp_error fmt e


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
  | GCE_metadata

(* On Google Compute Engine, we don't need credentials *)

type access_token =
  { access_token : string
  ; expires_in : int
  }

type token_info =
  { credentials : credentials
  ; token : access_token
  ; created_at : float
  ; scopes : string list
  ; project_id : string
  }

let token_info_mvar : token_info option Lwt_mvar.t = Lwt_mvar.create None

let access_token_of_json (json : Yojson.Basic.t) : access_token =
  let open Yojson.Basic.Util in
  let access_token = json |> member "access_token" |> to_string in
  let expires_in = json |> member "expires_in" |> to_int in
  { access_token; expires_in }


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
  | _ ->
      raise
        (Type_error
           (Printf.sprintf "Unknown credentials type: %S" cred_type, json) )


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
  with
  | Yojson.Basic.Util.Type_error (_msg, _) ->
      CCResult.fail `Bad_credentials_format


let credentials_of_file (credentials_file : string) :
    (credentials, [> `No_credentials | `Bad_credentials_format ]) result Lwt.t =
  let open Lwt.Infix in
  Lwt_unix.file_exists credentials_file
  >>= fun exists ->
  if not exists
  then Lwt.return_error `No_credentials
  else
    Lwt_io.(with_file ~mode:input) credentials_file (fun input_chan ->
        Lwt_io.read_lines input_chan
        |> Lwt_stream.to_list
        >>= fun lines ->
        lines |> String.concat "\n" |> credentials_of_string |> Lwt.return )


let access_token_of_response
    ((resp, body) : Cohttp.Response.t * Cohttp_lwt.Body.t) :
    (access_token, [> `Bad_token_response of string ]) result Lwt.t =
  let open Lwt.Infix in
  match Cohttp.Response.status resp with
  | `OK ->
      Cohttp_lwt.Body.to_string body
      >>= fun body_str ->
      ( try
          body_str
          |> Yojson.Basic.from_string
          |> access_token_of_json
          |> Lwt.return_ok
        with
      | Yojson.Basic.Util.Type_error (_msg, _) ->
          Lwt.return_error (`Bad_token_response body_str) )
  | _ ->
      Cohttp_lwt.Body.to_string body
      >>= fun body_str ->
      L.err (fun m -> m "response: %s" body_str)
      >>= fun () -> Lwt.return_error (`Bad_token_response body_str)


let access_token_of_credentials
    (scopes : string list) (credentials : credentials) :
    (access_token, [> `Bad_token_response of string ]) result Lwt.t =
  let open Lwt_result.Infix in
  let request =
    match credentials with
    | Authorized_user c ->
        let token_uri =
          Uri.make
            ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:"oauth2/v4/token"
        in
        let params =
          [ ("client_id", [ c.client_id ])
          ; ("client_secret", [ c.client_secret ])
          ; ("refresh_token", [ c.refresh_token ])
          ; ("grant_type", [ "refresh_token" ])
          ]
        in
        Cohttp_lwt_unix.Client.post_form token_uri ~params |> Lwt_result.ok
    | Service_account c ->
        let now = Unix.time () in
        Cstruct.of_string c.private_key
        |> X509.Private_key.decode_pem
        |> CCResult.map_err (function `Msg msg -> `Bad_credentials_priv_key msg)
        |> Lwt.return
        >>= fun (`RSA priv_key) ->
        let jwk = Jose.Jwk.make_priv_rsa priv_key in
        let header = Jose.Header.make_header ~typ:"JWT" jwk in
        let payload =
          Jose.Jwt.empty_payload
          |> Jose.Jwt.add_claim "iss" (`String c.client_email)
          |> Jose.Jwt.add_claim "scope" (`String (String.concat " " scopes))
          |> Jose.Jwt.add_claim "aud" (`String c.token_uri)
          |> Jose.Jwt.add_claim "iat" (`String (Printf.sprintf "%.0f" now))
          |> Jose.Jwt.add_claim
               "exp"
               (`String (Printf.sprintf "%.0f" (now +. 3600.)))
        in
        Jose.Jwt.sign ~header ~payload jwk
        |> CCResult.map_err (function `Msg msg -> `Jwt_signing_error msg)
        |> Lwt.return
        >>= fun jwt ->
        let params =
          [ ("grant_type", [ "urn:ietf:params:oauth:grant-type:jwt-bearer" ])
          ; ("assertion", [ Jose.Jwt.to_string jwt ])
          ]
        in
        Cohttp_lwt_unix.Client.post_form (Uri.of_string c.token_uri) ~params
        |> Lwt_result.ok
    | GCE_metadata ->
        let uri =
          Printf.sprintf
            "%s/instance/service-accounts/default/token"
            Compute_engine.Metadata.metadata_root
          |> Uri.of_string
        in
        Cohttp_lwt_unix.Client.get
          uri
          ~headers:Compute_engine.Metadata.metadata_headers
        |> Lwt_result.ok
  in
  request >>= access_token_of_response


let project_id_of_credentials (credentials : credentials) : string option =
  match credentials with
  | Service_account { project_id; _ } ->
      Some project_id
  | Authorized_user _ | GCE_metadata ->
      None


let discover_project_id (credentials : credentials) : string option =
  CCOpt.choice
    [ Sys.getenv_opt Environment_vars.google_project_id
    ; project_id_of_credentials credentials
    ]


let discover_credentials_with (discovery_mode : discovery_mode) =
  let open Lwt.Infix in
  L.debug (fun m ->
      m "Attempting authentication using %a" pp_discovery_mode discovery_mode )
  >>= fun () ->
  match discovery_mode with
  | Discover_credentials_path_from_env ->
      let credentials_file =
        Sys.getenv_opt Environment_vars.google_application_credentials
      in
      ( match credentials_file with
      | None ->
          Lwt.return_error `No_credentials
      | Some credentials_file ->
          let open Lwt_result.Infix in
          credentials_of_file credentials_file
          >>= fun credentials ->
          discover_project_id credentials
          |> CCOpt.to_result `No_project_id
          |> Lwt.return
          >>= fun project_id -> Lwt_result.return (credentials, project_id) )
  | Discover_credentials_json_from_env ->
      let credentials_json =
        Sys.getenv_opt Environment_vars.google_application_credentials_json
      in
      ( match credentials_json with
      | None ->
          Lwt.return_error `No_credentials
      | Some json_str ->
          let open Lwt_result.Infix in
          credentials_of_string json_str
          |> Lwt.return
          >>= fun credentials ->
          discover_project_id credentials
          |> CCOpt.to_result `No_project_id
          |> Lwt.return
          >>= fun project_id -> Lwt_result.return (credentials, project_id) )
  | Discover_credentials_from_cloud_sdk_path ->
      let open Lwt_result.Infix in
      credentials_of_file Paths.application_default_credentials
      >>= fun credentials ->
      ( match discover_project_id credentials with
      | Some project_id ->
          Lwt_result.return (credentials, project_id)
      | None ->
          Cloud_sdk.get_project_id ()
          |> Lwt_result.ok
          >>= fun project_id -> Lwt_result.return (credentials, project_id) )
  | Discover_credentials_from_gce_metadata ->
      let ping =
        Lwt.catch
          (fun () ->
            Compute_engine.Metadata.ping ()
            >>= fun (resp, _body) ->
            match Cohttp.Response.status resp with
            | `OK when Compute_engine.Metadata.response_has_metadata_header resp
              ->
                let open Lwt_result.Infix in
                let credentials = GCE_metadata in
                let project_id = discover_project_id credentials in
                ( match project_id with
                | Some project_id ->
                    Lwt_result.return project_id
                | None ->
                    Compute_engine.Metadata.get_project_id () )
                >>= fun project_id -> Lwt.return_ok (credentials, project_id)
            | _ ->
                Lwt.return_error `No_credentials )
          (fun _exn -> Lwt.return_error `No_credentials)
      in
      let timeout =
        Lwt_unix.sleep Compute_engine.Metadata.metadata_default_timeout
        >>= fun () -> Lwt.return_error `No_credentials
      in
      Lwt.pick [ ping; timeout ]


let rec first_ok ~(error : 'e) (fs : (unit -> ('a, 'e) result Lwt.t) list) :
    ('a, 'e) result Lwt.t =
  match fs with
  | [] ->
      Lwt.return_error error
  | t :: fs ->
      let open Lwt.Infix in
      t ()
      >>= (function
      | Ok x -> Lwt.return_ok x | Error error -> first_ok ~error fs )


let discover_credentials () : (credentials * string, [> error ]) Lwt_result.t =
  [ Discover_credentials_path_from_env
  ; Discover_credentials_json_from_env
  ; Discover_credentials_from_cloud_sdk_path
  ; Discover_credentials_from_gce_metadata
  ]
  |> List.map (fun discovery_mode () ->
         discover_credentials_with discovery_mode )
  |> first_ok ~error:`No_credentials


let get_access_token ?(scopes : string list = []) () :
    (token_info, [> error ]) Lwt_result.t =
  let get_new_access_token scopes =
    let open Lwt_result.Infix in
    discover_credentials ()
    >>= fun (credentials, project_id) ->
    access_token_of_credentials scopes credentials
    >>= fun access_token ->
    L.info (fun m -> m "Authenticated OK (project: %s)" project_id)
    |> Lwt_result.ok
    >|= fun () ->
    { credentials
    ; token = access_token
    ; created_at = Unix.time ()
    ; scopes
    ; project_id
    }
  in
  let has_requested_scopes token_info =
    CCList.subset ~eq:String.equal scopes token_info.scopes
  in
  let is_expired token_info =
    Unix.time ()
    > token_info.created_at +. float_of_int token_info.token.expires_in -. 30.
  in
  let open Lwt.Infix in
  Lwt_mvar.take token_info_mvar
  >>= (function
        | Some token_info
          when has_requested_scopes token_info && not (is_expired token_info) ->
            Lwt.return_ok token_info
        | Some token_info ->
            ( if is_expired token_info
            then L.debug (fun m -> m "Re-authenticating: Token is expired")
            else
              L.debug (fun m ->
                  m "Re-authenticating: Token does not have required scopes" )
            )
            >>= fun () ->
            get_new_access_token
              (CCList.union ~eq:String.equal token_info.scopes scopes)
        | None ->
            get_new_access_token scopes )
  >>= fun token_info_result ->
  Lwt_mvar.put token_info_mvar (CCResult.to_opt token_info_result)
  >>= fun () -> Lwt.return token_info_result


let get_project_id (scopes : string list) =
  let open Lwt_result.Infix in
  get_access_token ~scopes ()
  >>= fun token_info -> Lwt.return_ok token_info.project_id
