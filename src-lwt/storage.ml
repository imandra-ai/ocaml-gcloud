include Gcloud.Storage.Make (Async_task_lwt) (Client_cohttp_lwt)

let ok = Lwt_result.ok

(* Streaming variants, specific to cohttp-lwt bodies. *)

let get_object_stream (bucket_name : string) (object_path : string) :
    (string Lwt_stream.t, [> Error.t ]) result Lwt.t =
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
      Cohttp_lwt.Body.to_string body |> ok >>= fun body ->
      Error.of_response_status_code_and_body status_code body |> Lwt.return

let insert_object_stream ?if_generation_match ?if_generation_not_match
    bucket_name name (data : string Lwt_stream.t) :
    (object_, [> Error.t ]) result Lwt.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.devstorage_read_write ] ()
  >>= fun token_info ->
  Lwt.catch
    (fun () ->
      let uri =
        let query =
          [ ("name", [ name ]); ("uploadType", [ "media" ]) ]
          @ Gcloud.Storage.generation_query ~if_generation_match
              ~if_generation_not_match
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
      let body = Cohttp_lwt.Body.of_stream data in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post uri ~headers ~body >>= Util.consume_body |> ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> Error.parse_body_json object__of_yojson body |> Lwt.return
  | status_code ->
      Error.of_response_status_code_and_body status_code body |> Lwt.return
