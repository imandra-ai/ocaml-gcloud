(* https://developers.google.com/identity/protocols/OAuth2#serviceaccount *)
(* https://github.com/GoogleCloudPlatform/google-auth-library-python/blob/7e1270b1e5a99171fee4abfef6a4b9217ed378d7/google/auth/_default.py#L186 *)

(** Credential and token types, plus the pure parts of credential discovery.
    Fetching tokens is left to a backend (see [Gcloud_lwt.Auth]). *)

let src = Logs.Src.create "gcloud.auth"

module Log = (val Logs.src_log src : Logs.LOG)

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
      ( string,
        [> `Bad_subject_token_response of Cohttp.Response.t * string ] )
      result =
    match Cohttp.Response.status resp with
    | `OK -> (
        match t.credential_source.format.type_ with
        | `Json -> (
            try
              Ok
                (body_str |> Yojson.Basic.from_string |> subject_token_of_json t)
            with Yojson.Basic.Util.Type_error (msg, _) ->
              Log.debug (fun m -> m "Type_error: %s" msg);
              Error (`Bad_subject_token_response (resp, body_str))))
    | _ ->
        Log.err (fun m -> m "response: %s" body_str);
        Error (`Bad_subject_token_response (resp, body_str))
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

let access_token_of_json (json : Yojson.Basic.t) :
    (Access_token.t, [> `Bad_token_response of string ]) result =
  let open Yojson.Basic.Util in
  try
    let access_token = json |> member "access_token" |> to_string in
    let expires_in = json |> member "expires_in" |> to_int in
    Ok (Access_token.make ~access_token ~expires_in ())
  with Yojson.Basic.Util.Type_error (_msg, _) ->
    Error (`Bad_token_response Yojson.Basic.(to_string json))

(** Response of the IAM [generateAccessToken] endpoint used for service
    account impersonation. It differs from the OAuth token responses: camel
    case fields, and an RFC 3339 [expireTime] instead of [expires_in]. *)
let impersonated_access_token_of_json (json : Yojson.Basic.t) :
    (Access_token.t, [> `Bad_token_response of string ]) result =
  let open CCResult.Infix in
  let* access_token, expire_time =
    try
      let open Yojson.Basic.Util in
      let access_token = json |> member "accessToken" |> to_string in
      let expire_time = json |> member "expireTime" |> to_string in
      Ok (access_token, expire_time)
    with Yojson.Basic.Util.Type_error (_msg, _) ->
      CCResult.fail (`Bad_token_response Yojson.Basic.(to_string json))
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
    | None -> CCResult.fail (`Bad_token_response Yojson.Basic.(to_string json))
    | Some expires_in -> Ok expires_in
  in
  Ok
    (Access_token.make ~access_token ~expires_in
       ~additional_refresh_scopes:[ Scopes.iam ] ())

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

let access_token_of_response ?(of_json = access_token_of_json)
    ((resp, body_str) : Cohttp.Response.t * string) :
    (Access_token.t, [> `Bad_token_response of string ]) result =
  match Cohttp.Response.status resp with
  | `OK -> body_str |> Yojson.Basic.from_string |> of_json
  | _ ->
      Log.err (fun m -> m "response: %s" body_str);
      Error (`Bad_token_response body_str)
