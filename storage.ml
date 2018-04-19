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

let get_object (bucket_name : string) (object_path : string) : string option Lwt.t =
  let open Lwt.Infix in
  Auth.get_access_token ~scopes:[Scopes.devstorage_read_only] () >>= fun token_info ->
  let uri = Uri.make ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:(Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name (Uri.pct_encode object_path))
      ~query:[("alt", ["media"])]
  in
  Cohttp_lwt_unix.Client.get uri
    ~headers:(Cohttp.Header.of_list
                ["Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token])
  >>= fun (resp, body) ->
  (* Lwt_log.notice_f "Response code: %s" (Cohttp.Response.status resp |> Cohttp.Code.string_of_status) >>= fun () -> *)
  match Cohttp.Response.status resp with
  | `Not_found -> Lwt.return_none
  | _ -> Cohttp_lwt.Body.to_string body >|= CCOpt.return


type list_objects_response =
  { kind : string
  ; nextPageToken: string
  ; prefixes: string list
  ; items : Yojson.Safe.json list
  } [@@deriving yojson]

(* type 'a resp = *)
(*   [ `Not_found of error_response *)
(*   | `Ok of 'a *)
(*   ] *)

type gcloud_error =
  [`Gcloud_error_resp of ([`Not_found] * error_response)]

type errors =
  [ gcloud_error
  | `Gcloud_auth_error of exn
  | `Json_parse_error of string
  | `Json_transform_error of string
  | `Network_error of exn
  | `Http_error of Cohttp.Code.status_code
  ]

let json_parse_err_or_json (body : Cohttp_lwt.Body.t) : (Yojson.Safe.json, [> `Json_parse_error of string]) Lwt_result.t =
  (Lwt.catch
     (fun () ->
        let open Lwt.Infix in
        (Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string >|= CCResult.pure))
     (fun e ->
        (`Json_parse_error (Printexc.to_string e))
        |> Lwt_result.fail
     )
  )

let json_transform_err_or (parse : Yojson.Safe.json -> ('a, string) result) (json : Yojson.Safe.json)
  : ('a, [> `Json_transform_error of string]) Lwt_result.t =
  match (parse json) with
   | Ok j -> Lwt_result.return j
   | Error e -> Lwt_result.fail (`Json_parse_error e)

let as_gcloud_error (error_type : 'a) (error_resp : 'b) : ('c, [> gcloud_error]) Lwt_result.t =
  Lwt_result.fail (`Gcloud_error_resp (error_type, error_resp))

let list_objects (bucket_name : string) : (list_objects_response, errors) Lwt_result.t =
  let open Lwt_result.Infix in
  (Lwt.catch
     (fun () ->
        Auth.get_access_token ~scopes:[Scopes.devstorage_read_only] ()
        |> Lwt_result.ok
     )
     (fun e ->
        (`Gcloud_auth_error e)
        |> Lwt_result.fail
     )
  )
  >>= fun token_info ->
  (Lwt.catch
     (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "storage/v1/b/%s/o" bucket_name)
        in
        (Cohttp_lwt_unix.Client.get uri
           ~headers:(Cohttp.Header.of_list
                       ["Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token]))
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
    json_parse_err_or_json body >>= json_transform_err_or list_objects_response_of_yojson

  | `Not_found ->
    json_parse_err_or_json body >>= json_transform_err_or error_response_of_yojson >>= (as_gcloud_error `Not_found)

  | x ->
    Lwt_result.fail (`Http_error x)
