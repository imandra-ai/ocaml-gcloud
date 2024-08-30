let ok = Lwt_result.ok

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

  let insert ?project_id ~(rule : rule) () : (string, [> Error.t ]) Lwt_result.t
      =
    let open Lwt_result.Infix in
    Common.get_access_token ~scopes:[ Scopes.cloud_platform; Scopes.compute ] ()
    >>= fun token_info ->
    Common.get_project_id ?project_id ~token_info () >>= fun project_id ->
    Lwt.catch
      (fun () ->
        let uri =
          Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
            ~path:
              (Printf.sprintf "compute/v1/projects/%s/global/firewalls"
                 project_id)
        in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
              ("Content-Type", "application/json");
            ]
        in
        let body_str = rule |> rule_to_yojson |> Yojson.Safe.to_string in
        let body = body_str |> Cohttp_lwt.Body.of_string in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post uri ~body ~headers
        >>= Util.consume_body |> ok)
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt_result.return body
    | status_code -> Error.of_response_status_code_and_body status_code body

  let delete ?project_id ~(name : string) () :
      (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in
    Common.get_access_token ~scopes:[ Scopes.cloud_platform; Scopes.compute ] ()
    >>= fun token_info ->
    Common.get_project_id ?project_id ~token_info () >>= fun project_id ->
    Lwt.catch
      (fun () ->
        let uri =
          Uri.make () ~scheme:"https" ~host:"www.googleapis.com"
            ~path:
              (Printf.sprintf "compute/v1/projects/%s/global/firewalls/%s"
                 project_id name)
        in
        let headers =
          Cohttp.Header.of_list
            [
              ( "Authorization",
                Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
            ]
        in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.delete uri ~headers >>= Util.consume_body |> ok)
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt_result.return body
    | status_code -> Error.of_response_status_code_and_body status_code body
end
