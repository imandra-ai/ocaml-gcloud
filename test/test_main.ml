let stackdriver_errors_tests : unit Alcotest.test_case list =
  [( "report"
  , `Quick
  , (fun () ->
       let runner () =

         (* https://cloud.google.com/error-reporting/docs/formatting-error-messages *)
         Gcloud.Stackdriver_errors.report
           { event_time = None
           ; message = (Gcloud.Stackdriver_errors.stackdriver_nodejs_format ~pos:[%here] "Error: gcloud error report test message")
           ; context = Some
                 { http_request = None
                 ; user = None
                 ; source_references = []
                 ; report_location = None
                 }
           ; service_context =
               { service = "ocaml-gcloud test suite"
               ; version = "123"
               ; resource_type = None
               }
           }

       in
       match Lwt_main.run (runner ()) with
       | Ok () -> ()
       | Error e ->
         Alcotest.failf "Error:\n%a" Gcloud.Error.pp e
    ))]

let () =
  Alcotest.run "gcloud"
    [ ( "stackdriver errors"
      , stackdriver_errors_tests )]
