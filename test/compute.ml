let tests : unit Alcotest_lwt.test_case list =
  [ (* Manually tested *)
    (* Alcotest_lwt.test_case "firewalls" `Quick *)
    (*  (fun _ () -> *)
    (*     let open Lwt.Infix in *)

    (*     (\* gcloud compute firewall-rules create dave-strikes-back --source-tags=gke-try-imandra-dev-cluster-pool-2-d618ee36-p7wt --allow=tcp:32125 --source-ranges=0.0.0.0/0 *\) *)

    (*     let rule = *)
    (*       let open Gcloud.Compute.FirewallRules in *)
    (*       { name = "ocaml-gcloud-test-rule" *)
    (*       ; source_tags = ["gke-try-imandra-dev-cluster-pool-2-d618ee36-p7wt"] *)
    (*       ; source_ranges = ["0.0.0.0/0"] *)
    (*       ; description = None *)
    (*       ; network = None *)
    (*       ; allowed = *)
    (*           [ { ip_protocol = "tcp" *)
    (*             ; ports = ["32125"] *)
    (*             } *)
    (*           ] *)
    (*       } *)

    (*     in *)

    (*     Gcloud.Compute.FirewallRules.insert *)
    (*       ~project:"imandra-dev" ~rule *)

    (*     >>= function *)
    (*     | Ok _ -> *)
    (*       Alcotest.(check string) "correct name" "ok" "ok" |> Lwt.return >>= fun () -> *)
    (*       Lwt_unix.sleep 10.0 >>= fun () -> *)
    (*       begin *)

    (*         Gcloud.Compute.FirewallRules.delete *)
    (*           ~project:"imandra-dev" ~name:"ocaml-gcloud-test-rule" >>= function *)
    (*         | Ok _ -> *)
    (*           Alcotest.(check string) "correct name" "ok" "ok" |> Lwt.return *)

    (*         | Error e -> *)
    (*           Alcotest.failf "Error:\n%a" Gcloud.Error.pp e *)
    (*       end *)

    (*     | Error e -> *)
    (*       Alcotest.failf "Error:\n%a" Gcloud.Error.pp e *)
    (*  ) *) ]
