let () =
  Alcotest.run "gcloud"
    [ ( "stackdriver errors", Gcloud_tests.Stackdriver_errors.tests )
    ; ( "container", Gcloud_tests.Container.tests )
    ]
