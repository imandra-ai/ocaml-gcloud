module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
  let compute = "https://www.googleapis.com/auth/cloud-platform"
  let compute_readonly = "https://www.googleapis.com/auth/cloud-platform"
end

module FirewallRules = struct
  [@@@warning "-39"]

  type allowed = {
    ip_protocol : string; [@key "IPProtocol"]
    ports : string list;
  }
  [@@deriving yojson]

  type rule = {
    name : string;
    description : string option; [@default None]
    network : string option; [@default None]
    source_ranges : string list; [@key "sourceRanges"]
    source_tags : string list; [@key "sourceTags"]
    allowed : allowed list;
  }
  [@@deriving yojson]

  [@@@warning "+39"]
end

module Make
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  type 'a task = 'a Async.t

  module R = Async_task_result.Make (Async)
  module Req = Request.Make (Async) (Client)
  module Scopes = Scopes

  module FirewallRules = struct
    include FirewallRules

    let insert ?project_id ~(rule : rule) () :
        (string, [> Error.t ]) result task =
      let open R.Infix in
      Client.get_access_token
        ~scopes:[ Scopes.cloud_platform; Scopes.compute ]
        ()
      >>= fun token_info ->
      Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
      let uri =
        Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
          ~path:
            (Printf.sprintf "compute/v1/projects/%s/global/firewalls" project_id)
      in
      let headers =
        Cohttp.Header.of_list
          [ Req.bearer token_info; ("Content-Type", "application/json") ]
      in
      let body = rule |> rule_to_yojson |> Yojson.Safe.to_string in
      Req.call ~meth:`POST ~headers ~body uri >>= fun (status, body) ->
      match status with
      | `OK -> R.return body
      | status_code ->
          R.lift (Error.of_response_status_code_and_body status_code body)

    let delete ?project_id ~(name : string) () :
        (string, [> Error.t ]) result task =
      let open R.Infix in
      Client.get_access_token
        ~scopes:[ Scopes.cloud_platform; Scopes.compute ]
        ()
      >>= fun token_info ->
      Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
      let uri =
        Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
          ~path:
            (Printf.sprintf "compute/v1/projects/%s/global/firewalls/%s"
               project_id name)
      in
      let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
      Req.call ~meth:`DELETE ~headers uri >>= fun (status, body) ->
      match status with
      | `OK -> R.return body
      | status_code ->
          R.lift (Error.of_response_status_code_and_body status_code body)
  end
end
