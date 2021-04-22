let () =
  Logs.set_level (Some Logs.Debug) ;
  Logs.set_reporter (Logs_fmt.reporter ())


let () = Mirage_crypto_rng_unix.initialize ()

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "gcloud"
       [ ("stackdriver errors", Gcloud_tests.Stackdriver_errors.tests)
       ; ("container", Gcloud_tests.Container.tests)
       ; ("compute", Gcloud_tests.Compute.tests)
       ]
