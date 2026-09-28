let consume_body ((resp, body) : Cohttp.Response.t * Cohttp_lwt.Body.t) :
    (Cohttp.Response.t * string) Lwt.t =
  let open Lwt.Syntax in
  let* body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (resp, body_str)

let drain_body ((resp, body) : Cohttp.Response.t * Cohttp_lwt.Body.t) :
    Cohttp.Response.t Lwt.t =
  let open Lwt.Syntax in
  let* () = Cohttp_lwt.Body.drain_body body in
  Lwt.return resp
