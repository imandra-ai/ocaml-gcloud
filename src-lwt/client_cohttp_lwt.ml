type 'a task = 'a Lwt.t

let call ~meth ~headers ?body uri =
  let open Lwt.Syntax in
  let body = CCOption.map Cohttp_lwt.Body.of_string body in
  let* resp, body = Cohttp_lwt_unix.Client.call ?body ~headers meth uri in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (Cohttp.Response.status resp, body)

let get_access_token ~scopes () = Common.get_access_token ~scopes ()
let get_project_id = Common.get_project_id
