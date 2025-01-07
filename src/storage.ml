let ok = Lwt_result.ok

module Scopes = struct
  let devstorage_read_only =
    "https://www.googleapis.com/auth/devstorage.read_only"

  let devstorage_read_write =
    "https://www.googleapis.com/auth/devstorage.read_write"
end

type object_ = {
  name : string;
  time_created : string; [@key "timeCreated"]
  id : string; (* Other fields not parsed currently *)
}
[@@deriving yojson { strict = false }]
(** https://cloud.google.com/storage/docs/json_api/v1/objects#resource *)

let get_object_stream (bucket_name : string) (object_path : string) :
    (string Lwt_stream.t, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
  >>= fun token_info ->
  Lwt.catch
    (fun () ->
      let uri =
        Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
          ~path:
            (Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name
               (Uri.pct_encode object_path))
          ~query:[ ("alt", [ "media" ]) ]
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "Authorization",
              Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
          ]
      in
      Cohttp_lwt_unix.Client.get uri ~headers |> Lwt_result.ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> Cohttp_lwt.Body.to_stream body |> Lwt_result.return
  | status_code ->
      Cohttp_lwt.Body.to_string body
      |> ok
      >>= Error.of_response_status_code_and_body status_code

let get_object (bucket_name : string) (object_path : string) :
    (string, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  get_object_stream bucket_name object_path >>= fun stream ->
  Lwt_stream.to_list stream |> Lwt.map (String.concat "") |> Lwt_result.ok

let insert_object_ ~if_generation_match ~if_generation_not_match bucket_name
    name (body : Cohttp_lwt.Body.t) : (object_, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
  >>= fun token_info ->
  Lwt.catch
    (fun () ->
      let uri =
        let query =
          List.concat
            [
              [ ("name", [ name ]); ("uploadType", [ "media" ]) ];
              (match if_generation_match with
              | Some v -> [ ("ifGenerationMatch", [ string_of_int v ]) ]
              | None -> []);
              (match if_generation_not_match with
              | Some v -> [ ("ifGenerationNotMatch", [ string_of_int v ]) ]
              | None -> []);
            ]
        in
        Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
          ~path:(Printf.sprintf "upload/storage/v1/b/%s/o" bucket_name)
          ~query
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "Authorization",
              Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
          ]
      in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post uri ~headers ~body >>= Util.consume_body |> ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> Error.parse_body_json object__of_yojson body |> Lwt.return
  | status_code -> Error.of_response_status_code_and_body status_code body

let insert_object ?if_generation_match ?if_generation_not_match bucket_name name
    (data : string) : (object_, [> Error.t ]) Lwt_result.t =
  let body = Cohttp_lwt.Body.of_string data in
  insert_object_ ~if_generation_match ~if_generation_not_match bucket_name name
    body

let insert_object_stream ?if_generation_match ?if_generation_not_match
    bucket_name name (data : string Lwt_stream.t) :
    (object_, [> Error.t ]) Lwt_result.t =
  let body = Cohttp_lwt.Body.of_stream data in
  insert_object_ ~if_generation_match ~if_generation_not_match bucket_name name
    body

type rewrite_object_response = {
  kind : string;
  total_bytes_rewritten : string; [@key "totalBytesRewritten"]
  object_size : string; [@key "objectSize"]
  done_ : bool; [@key "done"]
  rewrite_token : string option; [@key "rewriteToken"] [@default None]
  resource : Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

(** NOTE: Multiple request rewrites not currently implemented.
    https://cloud.google.com/storage/docs/json_api/v1/objects/rewrite
 *)
let rewrite_object source_bucket source_object destination_bucket
    destination_object : (rewrite_object_response, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
  >>= fun token_info ->
  Lwt.catch
    (fun () ->
      let uri =
        let source_object = Uri.pct_encode source_object in
        let destination_object = Uri.pct_encode destination_object in
        Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
          ~path:
            (Printf.sprintf "storage/v1/b/%s/o/%s/rewriteTo/b/%s/o/%s"
               source_bucket source_object destination_bucket destination_object)
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "Authorization",
              Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
          ]
      in
      let body = Cohttp_lwt.Body.empty in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post uri ~headers ~body >>= Util.consume_body |> ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
      Error.parse_body_json rewrite_object_response_of_yojson body |> Lwt.return
  | status_code -> Error.of_response_status_code_and_body status_code body

[@@@warning "-39"]

type list_objects_response = {
  kind : string;
  next_page_token : string option; [@default None] [@key "nextPageToken"]
  prefixes : string list; [@default []]
  items : object_ list; [@default []]
}
[@@deriving yojson]

[@@@warning "+39"]

let list_objects ?(delimiter : string option) ?(prefix : string option)
    ?(page_token : string option) ~(bucket_name : string) () :
    (list_objects_response, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
  >>= fun token_info ->
  Lwt.catch
    (fun () ->
      let query =
        List.concat
          [
            delimiter
            |> CCOption.map_or ~default:[] (fun d -> [ ("delimiter", [ d ]) ]);
            prefix
            |> CCOption.map_or ~default:[] (fun p -> [ ("prefix", [ p ]) ]);
            page_token
            |> CCOption.map_or ~default:[] (fun t -> [ ("pageToken", [ t ]) ]);
          ]
      in
      let uri =
        Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
          ~path:(Printf.sprintf "storage/v1/b/%s/o" bucket_name)
          ~query
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "Authorization",
              Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
          ]
      in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.get uri ~headers >>= Util.consume_body |> ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
      Error.parse_body_json list_objects_response_of_yojson body |> Lwt.return
  | status_code -> Error.of_response_status_code_and_body status_code body

let delete_object ?if_generation_match ?if_generation_not_match
    (bucket_name : string) (object_path : string) :
    (unit, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Syntax in
  let* token_info =
    Common.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
  in
  let* resp, body =
    Lwt.catch
      (fun () ->
        let uri =
          let query =
            List.concat
              [
                (match if_generation_match with
                | Some v -> [ ("ifGenerationMatch", [ string_of_int v ]) ]
                | None -> []);
                (match if_generation_not_match with
                | Some v -> [ ("ifGenerationNotMatch", [ string_of_int v ]) ]
                | None -> []);
              ]
          in
          Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
            ~path:
              (Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name
                 (Uri.pct_encode object_path))
            ~query
        in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
            ]
        in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.delete uri ~headers >>= Util.consume_body |> ok)
      (fun e -> Lwt_result.fail (`Network_error e))
  in
  match Cohttp.Response.status resp with
  (* Deletion returns a 204 *)
  | Cohttp.Code.(#success_status) -> Lwt_result.return ()
  | status_code -> Error.of_response_status_code_and_body status_code body
