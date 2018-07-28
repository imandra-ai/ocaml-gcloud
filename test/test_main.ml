let stackdriver_errors_tests : unit Alcotest.test_case list =
  [( "report"
  , `Quick
  , (fun () ->
       let runner () =

         (* Needs OCAMLRUNPARAM=b *)
         let open Printexc in
         let raw_bt = Printexc.get_raw_backtrace () in
         let slot =
           Printexc.backtrace_slots raw_bt
           |> CCOpt.flat_map (fun s ->
               if Array.length s > 0 then
                 Some s.(0)
               else
                 None)
         in
         let formatted = Printexc.raw_backtrace_to_string raw_bt in

         Gcloud.Stackdriver_errors.report
           { event_time = None (* Some "" *) (* (Ptime_clock.now ()) *)
           ; message =
               (Printf.sprintf "Error: gcloud error report test message\n\r%s" formatted)
           ; context = Some
                 { http_request = None
                 ; user = None
                 ; source_references = []
                 ; report_location =
                     match (slot |> CCOpt.flat_map Printexc.Slot.location) with
                     | None ->
                       { file_path = "<unknown>"
                       ; line_number = 0
                       ; function_name = "<unknown>"
                       }
                     | Some l ->
                       { file_path = l.filename
                       ; line_number = l.line_number
                       ; function_name = (Printf.sprintf "%d:%d" l.start_char l.end_char)
                       }
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
