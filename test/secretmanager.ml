let tests : unit Alcotest_lwt.test_case list =
  [
    (* Requires access to the test secret *)
    Alcotest_lwt.test_case "projects.secrets.versions.access" `Quick
      (fun _ () ->
        let open Lwt.Infix in
        Gcloud.Secretmanager.V1.Projects.Secrets.Versions.access
          ~name:"projects/imandra-dev/secrets/test-secret/versions/latest"
        >>= function
        | Ok c ->
            Alcotest.(check string)
              "correct name"
              "projects/935198164717/secrets/test-secret/versions/1" c.name;
            Alcotest.(check string)
              "correct value" "hello test secret world!"
              (Base64.decode_exn c.payload.data);

            Lwt.return ()
        | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e);
  ]
