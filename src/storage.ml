module Scopes = struct
  let devstorage_read_only =
    "https://www.googleapis.com/auth/devstorage.read_only"

  let devstorage_read_write =
    "https://www.googleapis.com/auth/devstorage.read_write"
end

module Types = struct
  type object_ = {
    name : string;
    time_created : string; [@key "timeCreated"]
    id : string; (* Other fields not parsed currently *)
  }
  [@@deriving yojson { strict = false }]
  (** https://cloud.google.com/storage/docs/json_api/v1/objects#resource *)

  type rewrite_object_response = {
    kind : string;
    total_bytes_rewritten : string; [@key "totalBytesRewritten"]
    object_size : string; [@key "objectSize"]
    done_ : bool; [@key "done"]
    rewrite_token : string option; [@key "rewriteToken"] [@default None]
    resource : Yojson.Safe.t option; [@default None]
  }
  [@@deriving yojson]

  [@@@warning "-39"]

  type list_objects_response = {
    kind : string;
    next_page_token : string option; [@default None] [@key "nextPageToken"]
    prefixes : string list; [@default []]
    items : object_ list; [@default []]
  }
  [@@deriving yojson]

  [@@@warning "+39"]
end

include Types

(** Query parameters for the [ifGenerationMatch] / [ifGenerationNotMatch]
    preconditions. *)
let generation_query ~if_generation_match ~if_generation_not_match =
  List.concat
    [
      (match if_generation_match with
      | Some v -> [ ("ifGenerationMatch", [ string_of_int v ]) ]
      | None -> []);
      (match if_generation_not_match with
      | Some v -> [ ("ifGenerationNotMatch", [ string_of_int v ]) ]
      | None -> []);
    ]

(** Streaming variants ([get_object_stream], [insert_object_stream]) are
    specific to the Lwt backend; see [Gcloud_lwt.Storage]. *)
module Make
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  type 'a task = 'a Async.t

  module R = Async_task_result.Make (Async)
  module Req = Request.Make (Async) (Client)
  module Scopes = Scopes
  include Types

  let get_object (bucket_name : string) (object_path : string) :
      (string, [> Error.t ]) result task =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
    >>= fun token_info ->
    let uri =
      Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
        ~path:
          (Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name
             (Uri.pct_encode object_path))
        ~query:[ ("alt", [ "media" ]) ]
    in
    let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
    Req.call ~meth:`GET ~headers uri >>= fun (status, body) ->
    match status with
    | `OK -> R.return body
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)

  let insert_object ?if_generation_match ?if_generation_not_match bucket_name
      name (data : string) : (object_, [> Error.t ]) result task =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
    >>= fun token_info ->
    let uri =
      let query =
        [ ("name", [ name ]); ("uploadType", [ "media" ]) ]
        @ generation_query ~if_generation_match ~if_generation_not_match
      in
      Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
        ~path:(Printf.sprintf "upload/storage/v1/b/%s/o" bucket_name)
        ~query
    in
    let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
    Req.call ~meth:`POST ~headers ~body:data uri >>= fun (status, body) ->
    match status with
    | `OK -> R.lift (Error.parse_body_json object__of_yojson body)
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)

  (** NOTE: Multiple request rewrites not currently implemented.
      https://cloud.google.com/storage/docs/json_api/v1/objects/rewrite *)
  let rewrite_object source_bucket source_object destination_bucket
      destination_object : (rewrite_object_response, [> Error.t ]) result task =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
    >>= fun token_info ->
    let uri =
      let source_object = Uri.pct_encode source_object in
      let destination_object = Uri.pct_encode destination_object in
      Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
        ~path:
          (Printf.sprintf "storage/v1/b/%s/o/%s/rewriteTo/b/%s/o/%s"
             source_bucket source_object destination_bucket destination_object)
    in
    let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
    Req.call ~meth:`POST ~headers uri >>= fun (status, body) ->
    match status with
    | `OK ->
        R.lift (Error.parse_body_json rewrite_object_response_of_yojson body)
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)

  let list_objects ?(delimiter : string option) ?(prefix : string option)
      ?(page_token : string option) ~(bucket_name : string) () :
      (list_objects_response, [> Error.t ]) result task =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
    >>= fun token_info ->
    let query =
      List.concat
        [
          delimiter
          |> CCOption.map_or ~default:[] (fun d -> [ ("delimiter", [ d ]) ]);
          prefix |> CCOption.map_or ~default:[] (fun p -> [ ("prefix", [ p ]) ]);
          page_token
          |> CCOption.map_or ~default:[] (fun t -> [ ("pageToken", [ t ]) ]);
        ]
    in
    let uri =
      Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
        ~path:(Printf.sprintf "storage/v1/b/%s/o" bucket_name)
        ~query
    in
    let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
    Req.call ~meth:`GET ~headers uri >>= fun (status, body) ->
    match status with
    | `OK -> R.lift (Error.parse_body_json list_objects_response_of_yojson body)
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)

  let delete_object ?if_generation_match ?if_generation_not_match
      (bucket_name : string) (object_path : string) :
      (unit, [> Error.t ]) result task =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
    >>= fun token_info ->
    let uri =
      Uri.make () ~scheme:"https" ~host:"storage.googleapis.com"
        ~path:
          (Printf.sprintf "storage/v1/b/%s/o/%s" bucket_name
             (Uri.pct_encode object_path))
        ~query:(generation_query ~if_generation_match ~if_generation_not_match)
    in
    let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
    Req.call ~meth:`DELETE ~headers uri >>= fun (status, body) ->
    match status with
    (* Deletion returns a 204 *)
    | Cohttp.Code.(#success_status) -> R.return ()
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)
end
