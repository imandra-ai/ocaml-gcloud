module Scopes = struct
  let devstorage_read_only =
    "https://www.googleapis.com/auth/devstorage.read_only"
end

let get_object_stream (bucket_name : string) (object_path : string) :
    (string Lwt_stream.t, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Auth.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
  |> Lwt_result.map_error (fun e -> `Gcloud_auth_error e)
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
  | status_code -> Error.of_response_status_code_and_body status_code body

let get_object (bucket_name : string) (object_path : string) :
    (string, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  get_object_stream bucket_name object_path >>= fun stream ->
  Lwt_stream.to_list stream |> Lwt.map (String.concat "") |> Lwt_result.ok

[@@@warning "-39"]

type listed_object = {
  name : string;
  time_created : string; [@key "timeCreated"]
  id : string; (* Other fields not parsed currently *)
}
[@@deriving yojson { strict = false }]

type list_objects_response = {
  kind : string;
  next_page_token : string option; [@default None] [@key "nextPageToken"]
  prefixes : string list; [@default []]
  items : listed_object list; [@default []]
}
[@@deriving yojson]

[@@@warning "+39"]

let list_objects ?(delimiter : string option) ?(prefix : string option)
    ?(page_token : string option) ~(bucket_name : string) () :
    (list_objects_response, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Auth.get_access_token ~scopes:[ Scopes.devstorage_read_only ] ()
  |> Lwt_result.map_error (fun e -> `Gcloud_auth_error e)
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
      Cohttp_lwt_unix.Client.get uri ~headers |> Lwt_result.ok)
    (fun e -> Lwt_result.fail (`Network_error e))
  >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> Error.parse_body_json list_objects_response_of_yojson body
  | status_code -> Error.of_response_status_code_and_body status_code body
