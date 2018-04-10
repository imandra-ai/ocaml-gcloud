let get_object (bucket_name : string) (object_path : string) : string option Lwt.t =
  let open Lwt.Infix in
  Auth.get_access_token ~scopes:["https://www.googleapis.com/auth/devstorage.read_only"] () >>= fun token_info ->
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
