[@@@warning "-39"]

type api_error_item =
  { domain : string
  ; reason : string
  ; message : string
  ; location : string option [@default None]
  ; locationType : string option [@default None]
  }
[@@deriving yojson]

type api_error =
  { errors : api_error_item list [@default []]
  ; code : int
  ; message : string
  }
[@@deriving yojson { strict = false }]

type api_json_error = { error : api_error } [@@deriving yojson]

type api_error_response =
  | Json of api_json_error
  | Raw of string

[@@@warning "+39"]

type t =
  [ `Gcloud_auth_error of Auth.error
  | `Gcloud_api_error of Cohttp.Code.status_code * api_error_response
  | `Gcloud_retry_timeout of string
  | `Json_parse_error of string * string (* error, raw json *)
  | `Json_transform_error of string * Yojson.Safe.t (* error, raw json *)
  | `Network_error of exn
  ]

let pp fmt (error : t) =
  match error with
  | `Gcloud_auth_error error ->
      Format.fprintf fmt "Could not authenticate: %a" Auth.pp_error error
  | `Gcloud_api_error (status_code, api_error_response) ->
      Format.fprintf
        fmt
        "Gcloud API returned unexpected status code: %s (%s)"
        (Cohttp.Code.string_of_status status_code)
        ( match api_error_response with
        | Json j ->
            j |> api_json_error_to_yojson |> Yojson.Safe.to_string
        | Raw s ->
            s )
  | `Gcloud_retry_timeout msg ->
      Format.fprintf fmt "Gcloud retry timeout: %s" msg
  | `Json_parse_error (msg, json_str) ->
      Format.fprintf fmt "JSON parse error: %s (%s)" msg json_str
  | `Json_transform_error (msg, json_str) ->
      Format.fprintf
        fmt
        "JSON transform error: %s (%s)"
        msg
        (Yojson.Safe.to_string json_str)
  | `Network_error exn ->
      Format.fprintf fmt "Network error: %s" (Printexc.to_string exn)


let parse_body_json
    ?(gzipped = false)
    (transform : Yojson.Safe.t -> ('a, string) result)
    (body : Cohttp_lwt.Body.t) : ('a, [> t ]) Lwt_result.t =
  let open Lwt.Infix in
  Cohttp_lwt.Body.to_string body
  >>= fun body_str ->
  let body =
    if gzipped
    then
      Ezgzip.decompress body_str
      |> CCResult.map_err (fun _ ->
             `Json_parse_error ("gzip decode", "<gzipped>") )
    else Ok body_str
  in
  let parse_result =
    try body |> CCResult.map Yojson.Safe.from_string with
    | Yojson.Json_error msg ->
        Error (`Json_parse_error (msg, body_str))
    | e ->
        Error (`Json_parse_error (Printexc.to_string e, body_str))
  in

  parse_result
  |> CCResult.flat_map (fun json ->
         transform json
         |> CCResult.map_err (fun e -> `Json_transform_error (e, json)) )
  |> Lwt.return


let of_response_status_code_and_body
    (status_code : Cohttp.Code.status_code) (body : Cohttp_lwt.Body.t) :
    ('a, [> t ]) Lwt_result.t =
  let open Lwt.Infix in
  parse_body_json api_json_error_of_yojson body
  >>= function
  | Ok parsed_error ->
      Lwt_result.fail (`Gcloud_api_error (status_code, Json parsed_error))
  | Error (`Json_parse_error (_, body_str)) ->
      Lwt_result.fail (`Gcloud_api_error (status_code, Raw body_str))
  | Error e ->
      Lwt_result.fail
        (`Gcloud_api_error
          ( status_code
          , Raw (Format.asprintf "Error reading api error response: %a" pp e) )
          )
