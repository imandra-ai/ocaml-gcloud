let ok = Lwt_result.ok

module Scopes = struct
  let stackdriver_integration =
    "https://www.googleapis.com/auth/stackdriver-integration"
end

[@@@warning "-39"]

type service_context = {
  service : string;
  version : string;
  resource_type : string option; [@key "resourceType"] [@default None]
}
[@@deriving yojson]

type http_request_context = {
  method_ : string; [@key "method"]
  url : string;
  user_agent : string option; [@key "userAgent"] [@default None]
  referrer : string option; [@default None]
  response_status_code : int option; [@key "responseStatusCode"] [@default None]
  remote_ip : string option; [@key "remoteIp"] [@default None]
}
[@@deriving yojson]

type source_location = {
  file_path : string; [@key "filePath"]
  line_number : int; [@key "lineNumber"]
      (* Must be non-empty if no stackdriver-parseable stack trace in report_request.message *)
  function_name : string; [@key "functionName"]
}
[@@deriving yojson]

type source_reference = {
  repository : string;
  revision_id : string; [@key "revisionId"]
}
[@@deriving yojson]

type error_context = {
  http_request : http_request_context option;
      [@key "httpRequest"] [@default None]
  user : string option; [@default None]
      (* Must be set if no stackdriver-parseable stack trace in report_request.message *)
  report_location : source_location option;
      [@key "reportLocation"] [@default None]
  source_references : source_reference list; [@key "sourceReferences"]
}
[@@deriving yojson]

type report_request = {
  event_time : string option; [@key "eventTime"] [@default None]
  service_context : service_context; [@key "serviceContext"]
  message : string;
  context : error_context option; [@default None]
}
[@@deriving yojson]

[@@@warning "+39"]

let report ?project_id (report_request : report_request) :
    (unit, [> Error.t ]) Lwt_result.t =
  let open Lwt_result.Infix in
  Common.get_access_token ~scopes:[ Scopes.stackdriver_integration ] ()
  >>= fun token_info ->
  Common.get_project_id ?project_id ~token_info () >>= fun project_id ->
  Lwt.catch
    (fun () ->
      let uri =
        Uri.make () ~scheme:"https" ~host:"clouderrorreporting.googleapis.com"
          ~path:(Printf.sprintf "v1beta1/projects/%s/events:report" project_id)
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "Authorization",
              Printf.sprintf "Bearer %s" token_info.Auth.token.access_token );
          ]
      in
      let body_str =
        report_request |> report_request_to_yojson |> Yojson.Safe.to_string
      in
      let body = body_str |> Cohttp_lwt.Body.of_string in
      (let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post uri ~headers ~body >>= Util.consume_body |> ok)
      >>= fun (response, body) ->
      let status = Cohttp.Response.status response in
      match status with
      | `OK -> Lwt_result.return ()
      | _ -> Error.of_response_status_code_and_body status body)
    (fun e -> Lwt_result.fail (`Network_error e))

(** Generate an exception-shaped string to trigger Stackdriver error reporting.

    Supply [pos_opt] or [src_name_opt] to get a more informative stacktrace.
    [type_] should be a title case error name ending in "Exception" or "Error".

    We force our message to resemble a stacktrace to trigger Stackdriver error
    reporting.  Note, when using this string in a structured log, don't
    additionally set "@type" to
    "type.googleapis.com/google.devtools.clouderrorreporting.v1beta1.ReportedErrorEvent",
    which results in a slightly less informative Slack notification.

    https://cloud.google.com/error-reporting/docs/formatting-error-messages
    https://www.googlecloudcommunity.com/gc/Google-Cloud-s-operations-suite/How-to-show-an-error-title-in-Error-Reporting-slack-messages/m-p/661669
*)
let stackdriver_nodejs_format ?pos:(pos_opt : Lexing.position option)
    ?src_name:(src_name_opt : string option) ~(type_ : string) (msg : string) =
  let open Lexing in
  Printf.sprintf "%s: %s\n\tat <anonymous> " type_ msg
  ^
  match (pos_opt, src_name_opt) with
  | Some pos, _ ->
      Printf.sprintf "(%s:%d:%d)" pos.pos_fname pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
  | _, Some src_name -> Printf.sprintf "(%s)" src_name
  | _, _ -> "(unknown location)"
