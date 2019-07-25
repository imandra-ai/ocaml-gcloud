module Scopes = struct
  let pubsub = "https://www.googleapis.com/auth/pubsub"
end

module Subscriptions = struct

  type acknowledge_request = {
    ackIds : string list
  } [@@deriving yojson]

  let acknowledge ?project_id ~subscription_id ~ids : (unit, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.pubsub] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    let project_id =
      project_id |> CCOpt.get_or ~default:token_info.project_id
    in

    let request = { ackIds = ids } in

    Lwt.catch (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"pubsub.googleapis.com"
            ~path:(Printf.sprintf "/v1/projects/%s/subscriptions/%s:acknowledge" project_id subscription_id)

        in
        let body_str =
          request
          |> acknowledge_request_to_yojson
          |> Yojson.Safe.to_string
        in
        let body =
          body_str
          |> Cohttp_lwt.Body.of_string
        in
        let headers =
          Cohttp.Header.of_list
            [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
        in
        Logs_lwt.debug (fun m -> m "POST %a" Uri.pp_hum uri) |> Lwt_result.ok >>= fun () ->
        Cohttp_lwt_unix.Client.post uri ~headers ~body
        |> Lwt_result.ok
      )
      (fun e ->
         (`Network_error e)
         |> Lwt_result.fail)

    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      Lwt_result.ok ()
    | x ->
      Error.of_response_status_code_and_body x body

end
