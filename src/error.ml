[@@@warning "-39"]

type api_error_item =
  { domain : string
  ; reason : string
  ; message : string
  ; location : string option [@default None]
  ; locationType : string option [@default None]
  } [@@deriving yojson]

type api_error =
  { errors : api_error_item list
  ; code: int
  ; message: string
  } [@@deriving yojson]

type api_error_response =
  { error : api_error
  } [@@deriving yojson]

[@@@warning "+39"]

type t =
  [ `Gcloud_auth_error of Auth.error
  | `Gcloud_api_error of Cohttp.Code.status_code * api_error_response option
  | `Json_parse_error of (string * string) (* error, raw json *)
  | `Json_transform_error of (string * Yojson.Safe.json) (* error, raw json *)
  | `Network_error of exn
  ]

let pp fmt (error : t) =
  match error with
  | `Gcloud_auth_error error ->
    Format.fprintf fmt "Could not authenticate: %a" Auth.pp_error error
  | `Gcloud_api_error (status_code, api_error_response) ->
    Format.fprintf fmt "Gcloud API returned unexpected status code: %s (%s)"
      (Cohttp.Code.string_of_status status_code)
      (api_error_response
       |> CCOpt.map_or ~default:"Unable to decode error response body"
         (fun e -> Yojson.Safe.to_string (api_error_response_to_yojson e)))
  | `Json_parse_error (msg, json_str) ->
    Format.fprintf fmt "JSON parse error: %s (%s)" msg json_str
  | `Json_transform_error (msg, json_str) ->
    Format.fprintf fmt "JSON transform error: %s (%s)" msg (Yojson.Safe.to_string json_str)
  | `Network_error exn ->
    Format.fprintf fmt "Network error: %s" (Printexc.to_string exn)

let parse_body_json (transform : Yojson.Safe.json -> ('a, string) result) (body : Cohttp_lwt.Body.t) : ('a, [> t]) Lwt_result.t =
  let open Lwt.Infix in
  Cohttp_lwt.Body.to_string body >>= fun body_str ->

  let parse_result = try
      Ok (body_str |> Yojson.Safe.from_string)
    with
    | Yojson.Json_error msg -> Error (`Json_parse_error (msg, body_str))
    | e -> Error (`Json_parse_error (Printexc.to_string e, body_str))
  in

  parse_result
  |> CCResult.flat_map (fun json ->
      (transform json)
      |> CCResult.map_err (fun e -> `Json_transform_error (e, json))
    )
  |> Lwt.return

let of_response_status_code_and_body (status_code : Cohttp.Code.status_code) (body : Cohttp_lwt.Body.t) : ('a, [> t]) Lwt_result.t =
  let open Lwt_result.Infix in
  Lwt.Infix.(
    parse_body_json api_error_response_of_yojson body >|= function
    | Ok error -> Ok (Some error)
    | Error _ -> Ok None
  ) >>= fun error_opt ->
  Lwt_result.fail (`Gcloud_api_error (status_code, error_opt))
