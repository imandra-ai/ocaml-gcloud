let tests : unit Alcotest_lwt.test_case list =
  [
    Alcotest_lwt.test_case "report" `Quick (fun _ () ->
        let open Lwt.Infix in
        (* https://cloud.google.com/error-reporting/docs/formatting-error-messages *)
        Gcloud.Stackdriver_errors.report
          {
            event_time = None;
            message =
              Gcloud.Stackdriver_errors.stackdriver_nodejs_format ~pos:[%here]
                ~type_:"Error" "gcloud error report test message";
            context =
              Some
                {
                  http_request = None;
                  user = None;
                  source_references = [];
                  report_location = None;
                };
            service_context =
              {
                service = "ocaml-gcloud test suite";
                version = "123";
                resource_type = None;
              };
          }
        >>= function
        | Ok () -> Lwt.return ()
        | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e);
    Alcotest_lwt.test_case "report" `Quick (fun _ () ->
        let open Lwt.Infix in
        (* https://cloud.google.com/error-reporting/docs/formatting-error-messages *)
        let pos = [%here] in
        Gcloud.Stackdriver_errors.report
          {
            event_time = None;
            message =
              Gcloud.Stackdriver_errors.stackdriver_nodejs_format ~type_:"Error"
                "gcloud error report test message no pos in exception";
            context =
              Some
                {
                  http_request = None;
                  user = None;
                  source_references = [];
                  report_location =
                    Some
                      {
                        file_path = pos.pos_fname;
                        line_number = pos.pos_lnum;
                        function_name = "some_function";
                      };
                };
            service_context =
              {
                service = "ocaml-gcloud test suite";
                version = "123";
                resource_type = None;
              };
          }
        >>= function
        | Ok () -> Lwt.return ()
        | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e);
  ]
