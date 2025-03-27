module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
  let logging = "https://www.googleapis.com/auth/logging.admin"
  let logging_readonly = "https://www.googleapis.com/auth/logging.read"
end

module Entries = struct
  type filter = string option [@@deriving yojson]

  type body = { resource_names : string list; filter : filter }
  [@@deriving yojson]

  let list ?project_id ~(body : body) () : (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in
    Common.get_access_token ~scopes:[ Scopes.cloud_platform; Scopes.logging ] ()
    >>= fun token_info ->
    Common.get_project_id ?project_id ~token_info () >>= fun project_id ->
    Lwt.catch
      (fun () ->
        let uri =
          Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
            ~path:"v2/entries:list"
        in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
              ("Content-Type", "application/json");
            ]
        in
        let body_str =
          {
            body with
            resource_names =
              body.resource_names @ [ Format.sprintf "projects/%s" project_id ];
          }
          |> body_to_yojson |> Yojson.Safe.to_string
        in
        let body = body_str |> Cohttp_lwt.Body.of_string in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post uri ~body ~headers
        >>= Util.consume_body |> Lwt_result.ok)
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt_result.return body
    | status_code -> Error.of_response_status_code_and_body status_code body
end
