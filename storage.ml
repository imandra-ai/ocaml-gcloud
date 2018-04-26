module Scopes = struct
  let devstorage_read_only = "https://www.googleapis.com/auth/devstorage.read_only"
end

type error_response_errors_item =
  { domain : string
  ; reason : string
  ; message : string
  } [@@deriving yojson]

type error_response_error =
  { errors : error_response_errors_item list
  } [@@deriving yojson]

type error_response =
  { error : error_response_error
  ; code: int
  ; message: string
  } [@@deriving yojson]

type gcloud_error = ([`Not_found] * error_response)

let string_of_gcloud_error : gcloud_error -> string = function
  | `Not_found, e -> (Printf.sprintf "Not found: %s" (e |> error_response_to_yojson |> Yojson.Safe.to_string))

type errors =
  [ `Gcloud_error_resp of gcloud_error
  | `Gcloud_auth_error of exn
  | `Json_parse_error of exn
  | `Json_transform_error of exn
  | `Network_error of exn
  | `Http_error of Cohttp.Code.status_code
  ]

let string_of_error : errors -> string = function
  | `Http_error s -> (Printf.sprintf "HTTP Error: %s" (Cohttp.Code.string_of_status s))
  | `Network_error e -> (Printf.sprintf "Network Error: %s" (Printexc.to_string e))
  | `Json_transform_error e -> (Printf.sprintf "JSON Transform Error: %s" (Printexc.to_string e))
  | `Json_parse_error e -> (Printf.sprintf "JSON Parse Error: %s" (Printexc.to_string e))
  | `Gcloud_error_resp ge -> (Printf.sprintf "GCloud Error: %s" (string_of_gcloud_error ge))
  | `Gcloud_auth_error e -> (Printf.sprintf "GCloud Auth Error: %s" (Printexc.to_string e))

let pp_error : errors CCFormat.printer =
  CCFormat.of_to_string string_of_error

let json_parse_err_or_json (body : Cohttp_lwt.Body.t) : (Yojson.Safe.json, [> `Json_parse_error of exn]) Lwt_result.t =
  let open Lwt.Infix in
  (Lwt_result.catch
     (Cohttp_lwt.Body.to_string body >>= (Lwt.wrap1 Yojson.Safe.from_string)))
  |> (Lwt_result.map_err (fun e -> `Json_parse_error e))

exception Json_transform_error of string

let json_transform_err_or (transform : Yojson.Safe.json -> ('a, string) result) (json : Yojson.Safe.json)
  : ('a, [> `Json_transform_error of exn]) Lwt_result.t =
  Lwt_result.lift (transform json)
  |> (Lwt_result.map_err (fun e -> `Json_transform_error (Json_transform_error e)))

let as_gcloud_error (error_type : 'a) (error_resp : 'b) : ('c, [> `Gcloud_error_resp of gcloud_error]) Lwt_result.t =
  Lwt_result.fail (`Gcloud_error_resp (error_type, error_resp))


let get_object (bucket_name : string) (object_path : string) : (string, [> errors ]) Lwt_result.t =
  let open Lwt_result.Infix in

  (Auth.get_access_token ~scopes:[Scopes.devstorage_read_only] ()
   |> Lwt_result.catch
   |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e))

  >>= fun token_info ->
  (Lwt.catch
     (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name (Uri.pct_encode object_path))
            ~query:[("alt", ["media"])]
        in
        let headers =
          Cohttp.Header.of_list
            [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
        in
        Cohttp_lwt_unix.Client.get uri ~headers
        |> Lwt_result.ok
     ))
    (fun e ->
       (`Network_error e)
       |> Lwt_result.fail)

  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
    Cohttp_lwt.Body.to_string body |> Lwt_result.ok
  | `Not_found ->
    Cohttp_lwt.Body.to_string body |> Lwt_result.ok >>= fun message ->
    (** With alt=media, the API does not return a JSON error object, so we create one ourselves here. *)
    let error_response =
      { error =
          { errors =
              [ { domain = "global"
                ; reason = "notFound"
                ; message
                }
              ]
          }
      ; code = 404
      ; message
      }
    in
    as_gcloud_error `Not_found error_response
  | x ->
    Lwt_result.fail (`Http_error x)

type listed_object =
  { name : string
  ; time_created : string [@key "timeCreated"]
  ; id : string
  (* Other fields not parsed currently *)
  } [@@deriving yojson { strict = false }]

type list_objects_response =
  { kind : string
  ; nextPageToken: string option [@default None]
  ; prefixes: string list [@default []]
  ; items : listed_object list
  } [@@deriving yojson]

let list_objects (bucket_name : string) : (list_objects_response, [> errors]) Lwt_result.t =
  let open Lwt_result.Infix in

  (Auth.get_access_token ~scopes:[Scopes.devstorage_read_only] ()
   |> Lwt_result.catch
   |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e))

  >>= fun token_info ->
  (Lwt.catch
     (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "storage/v1/b/%s/o" bucket_name)
        in
        let headers =
          Cohttp.Header.of_list
            [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
        in
        (Cohttp_lwt_unix.Client.get uri ~headers:headers)
        |> Lwt_result.ok
     )
     (fun e ->
        (`Network_error e)
        |> Lwt_result.fail
     )
  )
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
    json_parse_err_or_json body
    >>= json_transform_err_or list_objects_response_of_yojson

  | `Not_found ->
    json_parse_err_or_json body
    >>= json_transform_err_or error_response_of_yojson
    >>= (as_gcloud_error `Not_found)

  | x ->
    Lwt_result.fail (`Http_error x)
