let tests : unit Alcotest_lwt.test_case list =
  [
    Alcotest_lwt.test_case "projects.locations.clusters.get" `Quick (fun _ () ->
        let open Lwt.Infix in
        Gcloud.Container.Projects.Locations.Clusters.get ~project:"imandra-dev"
          ~location:"europe-west1-c" ~cluster:"imandra-markets-dev-cluster"
        >>= function
        | Ok c ->
            Alcotest.(check string)
              "correct name" c.name "imandra-markets-dev-cluster"
            |> Lwt.return
        | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e);
  ]
