module Scopes = struct
  let pubsub = "https://www.googleapis.com/auth/pubsub"
end

module Subscriptions = struct
  let log_src_pull = Logs.Src.create "Gcloud.Pub_sub.Subscriptions.pull"

  type acknowledge_request = { ackIds : string list } [@@deriving yojson]

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
end

module Make
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  type 'a task = 'a Async.t

  module R = Async_task_result.Make (Async)
  module Req = Request.Make (Async) (Client)
  module Scopes = Scopes

  module Subscriptions = struct
    include Subscriptions

    let host = "pubsub.googleapis.com"

    let acknowledge ?project_id ~subscription_id ~ids () :
        (unit, [> Error.t ]) result task =
      let open R.Infix in
      Client.get_access_token ~scopes:[ Scopes.pubsub ] () >>= fun token_info ->
      Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
      let request = { ackIds = ids } in
      let uri =
        Uri.make () ~scheme:"https" ~host
          ~path:
            (Printf.sprintf "/v1/projects/%s/subscriptions/%s:acknowledge"
               project_id subscription_id)
      in
      let body =
        request |> acknowledge_request_to_yojson |> Yojson.Safe.to_string
      in
      let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
      Req.call ~meth:`POST ~headers ~body uri >>= fun (status, body) ->
      match status with
      | `OK -> R.return ()
      | x -> R.lift (Error.of_response_status_code_and_body x body)

    let pull ?project_id ~subscription_id ~max_messages
        ?(return_immediately = true) () :
        (received_messages, [> Error.t ]) result task =
      let open R.Infix in
      Client.get_access_token ~scopes:[ Scopes.pubsub ] () >>= fun token_info ->
      Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
      let request =
        { maxMessages = max_messages; returnImmediately = return_immediately }
      in
      let uri =
        Uri.make () ~scheme:"https" ~host
          ~path:
            (Printf.sprintf "/v1/projects/%s/subscriptions/%s:pull" project_id
               subscription_id)
      in
      let body = request |> pull_request_to_yojson |> Yojson.Safe.to_string in
      let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
      Logs.debug ~src:log_src_pull (fun m -> m "POST %a" Uri.pp_hum uri);
      Req.call ~meth:`POST ~headers ~body uri >>= fun (status, body) ->
      match status with
      | `OK ->
          R.lift (Error.parse_body_json received_messages_of_yojson body)
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
      | x -> R.lift (Error.of_response_status_code_and_body x body)
  end
end
