module Scopes = struct
  let pubsub = "https://www.googleapis.com/auth/pubsub"
end

module Subscriptions = struct
  let log_src_pull = Logs.Src.create "Gcloud.Pub_sub.Subscriptions.pull"

  type acknowledge_request = { ackIds : string list } [@@deriving yojson]

  let acknowledge ?project_id ~subscription_id ~ids () :
      (unit, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in
    Auth.get_access_token ~scopes:[ Scopes.pubsub ] ()
    |> Lwt_result.map_error (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->
    let project_id =
      project_id |> CCOption.get_or ~default:token_info.project_id
    in

    let request = { ackIds = ids } in

    Lwt.catch
      (fun () ->
        let uri =
          Uri.make () ~scheme:"https" ~host:"pubsub.googleapis.com"
            ~path:
              (Printf.sprintf "/v1/projects/%s/subscriptions/%s:acknowledge"
                 project_id subscription_id)
        in

        let body_str =
          request |> acknowledge_request_to_yojson |> Yojson.Safe.to_string
        in
        let body = body_str |> Cohttp_lwt.Body.of_string in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
            ]
        in
        Logs_lwt.debug (fun m -> m "POST %a" Uri.pp_hum uri) |> Lwt_result.ok
        >>= fun () ->
        Cohttp_lwt_unix.Client.post uri ~headers ~body |> Lwt_result.ok)
      (fun e -> `Network_error e |> Lwt_result.fail)
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt_result.return ()
    | x -> Error.of_response_status_code_and_body x body

  type pull_request = { returnImmediately : bool; maxMessages : int }
  [@@deriving yojson]

  type message = {
    data : string;
    message_id : string; [@key "messageId"]
    publish_time : string; [@key "publishTime"]
  }
  [@@deriving yojson { strict = false }]

  type received_message = { ack_id : string; [@key "ackId"] message : message }
  [@@deriving yojson]

  type received_messages = {
    received_messages : received_message list;
        [@key "receivedMessages"] [@default []]
  }
  [@@deriving yojson]

  let pull ?project_id ~subscription_id ~max_messages
      ?(return_immediately = true) () =
    let open Lwt_result.Infix in
    Auth.get_access_token ~scopes:[ Scopes.pubsub ] ()
    |> Lwt_result.map_error (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->
    let project_id =
      project_id |> CCOption.get_or ~default:token_info.project_id
    in

    let request =
      { maxMessages = max_messages; returnImmediately = return_immediately }
    in

    Lwt.catch
      (fun () ->
        let uri =
          Uri.make () ~scheme:"https" ~host:"pubsub.googleapis.com"
            ~path:
              (Printf.sprintf "/v1/projects/%s/subscriptions/%s:pull" project_id
                 subscription_id)
        in

        let body_str =
          request |> pull_request_to_yojson |> Yojson.Safe.to_string
        in
        let body = body_str |> Cohttp_lwt.Body.of_string in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
            ]
        in
        Logs_lwt.debug ~src:log_src_pull (fun m -> m "POST %a" Uri.pp_hum uri)
        |> Lwt_result.ok
        >>= fun () ->
        Cohttp_lwt_unix.Client.post uri ~headers ~body |> Lwt_result.ok)
      (fun e -> `Network_error e |> Lwt_result.fail)
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
        Error.parse_body_json received_messages_of_yojson body
        >|= fun { received_messages } ->
        let received_messages =
          received_messages
          |> List.map (fun ({ message; _ } as msg) ->
                 {
                   msg with
                   message =
                     { message with data = Base64.decode_exn message.data };
                 })
        in
        { received_messages }
    | x -> Error.of_response_status_code_and_body x body
end
