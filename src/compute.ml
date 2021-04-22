module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"

  let compute = "https://www.googleapis.com/auth/cloud-platform"

  let compute_readonly = "https://www.googleapis.com/auth/cloud-platform"
end

module FirewallRules = struct
  [@@@warning "-39"]

  type allowed =
    { ip_protocol : string [@key "IPProtocol"]
    ; ports : string list
    }
  [@@deriving yojson]

  type rule =
    { name : string
    ; description : string option [@default None]
    ; network : string option [@default None]
    ; source_ranges : string list [@key "sourceRanges"]
    ; source_tags : string list [@key "sourceTags"]
    ; allowed : allowed list
    }
  [@@deriving yojson]

  [@@@warning "+39"]

  let insert ~(project : string) ~(rule : rule) :
      (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in
    Auth.get_access_token ~scopes:[ Scopes.cloud_platform; Scopes.compute ] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->
    Lwt.catch
      (fun () ->
        let uri =
          Uri.make
            ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:
              (Printf.sprintf "compute/v1/projects/%s/global/firewalls" project)
        in
        let headers =
          Cohttp.Header.of_list
            [ ( "Authorization"
              , Printf.sprintf "Bearer %s" token_info.Auth.token.access_token )
            ; ("Content-Type", "application/json")
            ]
        in
        let body_str = rule |> rule_to_yojson |> Yojson.Safe.to_string in
        print_endline body_str ;
        let body = body_str |> Cohttp_lwt.Body.of_string in
        Cohttp_lwt_unix.Client.post uri ~body ~headers |> Lwt_result.ok )
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
        Lwt_result.ok (Cohttp_lwt.Body.to_string body)
    | status_code ->
        Error.of_response_status_code_and_body status_code body


  let delete ~(project : string) ~(name : string) :
      (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in
    Auth.get_access_token ~scopes:[ Scopes.cloud_platform; Scopes.compute ] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->
    Lwt.catch
      (fun () ->
        let uri =
          Uri.make
            ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:
              (Printf.sprintf
                 "compute/v1/projects/%s/global/firewalls/%s"
                 project
                 name )
        in
        let headers =
          Cohttp.Header.of_list
            [ ( "Authorization"
              , Printf.sprintf "Bearer %s" token_info.Auth.token.access_token )
            ]
        in
        Cohttp_lwt_unix.Client.delete uri ~headers |> Lwt_result.ok )
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
        Lwt_result.ok (Cohttp_lwt.Body.to_string body)
    | status_code ->
        Error.of_response_status_code_and_body status_code body
end
