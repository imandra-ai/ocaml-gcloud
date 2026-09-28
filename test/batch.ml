open Gcloud.Batch.V1
module Alpha = Gcloud.Batch.V1alpha

let json : Yojson.Safe.t Alcotest.testable =
  Alcotest.testable Yojson.Safe.pretty_print ( = )

let job_state : Job_state.t Alcotest.testable =
  Alcotest.testable Job_state.pp ( = )

let get_ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "Unexpected error: %s" e

let rec has_null : Yojson.Safe.t -> bool = function
  | `Null -> true
  | `Assoc kvs -> List.exists (fun (_, j) -> has_null j) kvs
  | `List js -> List.exists has_null js
  | _ -> false

(* A small but representative job spec built with the [make] constructors. *)
let sample_job =
  Job.make ~priority:50
    ~task_groups:
      [
        Task_group.make ~task_count:3 ~parallelism:2
          ~scheduling_policy:Scheduling_policy.AS_SOON_AS_POSSIBLE
          ~task_spec:
            (Task_spec.make
               ~runnables:
                 [
                   Runnable.make
                     ~script:
                       (Script.make ~text:"echo Hello ${BATCH_TASK_INDEX}" ())
                     ();
                   Runnable.make
                     ~container:
                       (Container.make ~image_uri:"busybox"
                          ~commands:[ "sh"; "-c"; "echo done" ]
                          ())
                     ~ignore_exit_status:true ();
                 ]
               ~compute_resource:
                 (Compute_resource.make ~cpu_milli:1000 ~memory_mib:512 ())
               ~max_retry_count:2 ~max_run_duration:"3600s"
               ~environment:(Environment.make ~variables:[ ("FOO", "bar") ] ())
               ())
          ();
      ]
    ~allocation_policy:
      (Allocation_policy.make
         ~instances:
           [
             Instance_policy_or_template.make
               ~policy:
                 (Instance_policy.make ~machine_type:"e2-standard-4"
                    ~provisioning_model:Provisioning_model.SPOT ())
               ();
           ]
         ~location:
           (Location_policy.make ~allowed_locations:[ "regions/us-central1" ] ())
         ())
    ~labels:[ ("env", "test") ]
    ~logs_policy:
      (Logs_policy.make ~destination:Logs_destination.CLOUD_LOGGING ())
    ()

(* A job as returned by the API, including output-only fields and an unknown
   field that must be tolerated. *)
let job_response =
  {|{
  "name": "projects/123456789/locations/us-central1/jobs/test-job",
  "uid": "j-0123abcd",
  "priority": "50",
  "taskGroups": [
    {
      "name": "projects/123456789/locations/us-central1/jobs/test-job/taskGroups/group0",
      "taskSpec": {
        "runnables": [{ "script": { "text": "echo hi" } }],
        "computeResource": { "cpuMilli": "1000", "memoryMib": "512" },
        "maxRetryCount": 1,
        "maxRunDuration": "3600s"
      },
      "taskCount": "2",
      "parallelism": "2",
      "schedulingPolicy": "AS_SOON_AS_POSSIBLE"
    }
  ],
  "allocationPolicy": {
    "instances": [
      { "policy": { "machineType": "e2-standard-2", "provisioningModel": "STANDARD" } }
    ],
    "location": { "allowedLocations": ["regions/us-central1"] }
  },
  "labels": { "env": "test" },
  "status": {
    "state": "SUCCEEDED",
    "statusEvents": [
      {
        "type": "STATUS_CHANGED",
        "description": "Job state is set from QUEUED to SCHEDULED for job projects/123456789/locations/us-central1/jobs/test-job.",
        "eventTime": "2025-09-28T10:00:00.000000000Z"
      },
      {
        "type": "STATUS_CHANGED",
        "description": "Task state is updated from RUNNING to SUCCEEDED on zones/us-central1-a/instances/123.",
        "eventTime": "2025-09-28T10:01:00.000000000Z",
        "taskState": "SUCCEEDED",
        "taskExecution": { "exitCode": 0 }
      }
    ],
    "taskGroups": {
      "group0": {
        "counts": { "SUCCEEDED": "2" },
        "instances": [
          {
            "machineType": "e2-standard-2",
            "provisioningModel": "STANDARD",
            "taskPack": "1",
            "bootDisk": { "type": "pd-balanced", "sizeGb": "30" }
          }
        ]
      }
    },
    "runDuration": "12.345s"
  },
  "createTime": "2025-09-28T09:59:00.000000000Z",
  "updateTime": "2025-09-28T10:01:00.000000000Z",
  "logsPolicy": { "destination": "CLOUD_LOGGING" },
  "someFutureField": { "x": 1 }
}|}

let tests : unit Alcotest_lwt.test_case list =
  [
    Alcotest_lwt.test_case_sync "Job.to_yojson" `Quick (fun () ->
        let open Yojson.Safe.Util in
        let j = Job.to_yojson sample_job in
        Alcotest.(check bool) "no nulls in request body" false (has_null j);
        Alcotest.(check json) "output-only name omitted" `Null (member "name" j);
        Alcotest.(check json)
          "int64 as string" (`String "50") (member "priority" j);
        let tg = member "taskGroups" j |> index 0 in
        Alcotest.(check json) "taskCount" (`String "3") (member "taskCount" tg);
        Alcotest.(check json)
          "schedulingPolicy enum" (`String "AS_SOON_AS_POSSIBLE")
          (member "schedulingPolicy" tg);
        Alcotest.(check json)
          "script text" (`String "echo Hello ${BATCH_TASK_INDEX}")
          (member "taskSpec" tg |> member "runnables" |> index 0
         |> member "script" |> member "text");
        Alcotest.(check json)
          "ignoreExitStatus" (`Bool true)
          (member "taskSpec" tg |> member "runnables" |> index 1
         |> member "ignoreExitStatus");
        Alcotest.(check json)
          "cpuMilli" (`String "1000")
          (member "taskSpec" tg |> member "computeResource" |> member "cpuMilli");
        Alcotest.(check json)
          "environment variables map"
          (`Assoc [ ("FOO", `String "bar") ])
          (member "taskSpec" tg |> member "environment" |> member "variables");
        Alcotest.(check json)
          "labels map"
          (`Assoc [ ("env", `String "test") ])
          (member "labels" j);
        Alcotest.(check json)
          "provisioningModel" (`String "SPOT")
          (member "allocationPolicy" j
          |> member "instances" |> index 0 |> member "policy"
          |> member "provisioningModel");
        Alcotest.(check json)
          "logs destination" (`String "CLOUD_LOGGING")
          (member "logsPolicy" j |> member "destination"));
    Alcotest_lwt.test_case_sync "Job round-trip" `Quick (fun () ->
        let j = Job.to_yojson sample_job in
        let job' = Job.of_yojson j |> get_ok in
        Alcotest.(check json) "stable" j (Job.to_yojson job');
        Alcotest.(check bool) "equal" true (sample_job = job'));
    Alcotest_lwt.test_case_sync "Job.of_yojson (API response)" `Quick (fun () ->
        let job =
          Yojson.Safe.from_string job_response |> Job.of_yojson |> get_ok
        in
        Alcotest.(check (option string)) "id" (Some "test-job") (Job.id job);
        Alcotest.(check job_state) "state" Job_state.SUCCEEDED (Job.state job);
        Alcotest.(check bool)
          "terminal" true
          (Job_state.is_terminal (Job.state job));
        Alcotest.(check (option int)) "priority" (Some 50) job.priority;
        Alcotest.(check (list (pair string string)))
          "labels"
          [ ("env", "test") ]
          job.labels;
        let tg = List.hd job.task_groups in
        Alcotest.(check (option int)) "taskCount" (Some 2) tg.task_count;
        Alcotest.(check (option int))
          "maxRetryCount" (Some 1) tg.task_spec.max_retry_count;
        Alcotest.(check (option int))
          "cpuMilli" (Some 1000)
          (Option.bind tg.task_spec.compute_resource (fun c -> c.cpu_milli));
        let status = Option.get job.status in
        Alcotest.(check int)
          "status events" 2
          (List.length status.status_events);
        let last = List.nth status.status_events 1 in
        Alcotest.(check (option int))
          "exit code" (Some 0)
          (Option.bind last.task_execution (fun e -> e.exit_code));
        Alcotest.(check bool)
          "task state" true
          (last.task_state = Some Task_state.SUCCEEDED);
        let group0 = List.assoc "group0" status.task_groups in
        Alcotest.(check (list (pair string int)))
          "counts"
          [ ("SUCCEEDED", 2) ]
          group0.counts;
        let inst = List.hd group0.instances in
        Alcotest.(check (option int)) "taskPack" (Some 1) inst.task_pack;
        Alcotest.(check (option int))
          "bootDisk sizeGb" (Some 30)
          (Option.bind inst.boot_disk (fun d -> d.size_gb)));
    Alcotest_lwt.test_case_sync "Operation.of_yojson" `Quick (fun () ->
        let op =
          Yojson.Safe.from_string
            {|{
  "name": "projects/123456789/locations/us-central1/operations/0b7a1c2d",
  "metadata": {
    "@type": "type.googleapis.com/google.cloud.batch.v1.OperationMetadata",
    "createTime": "2025-09-28T10:02:00.000000000Z",
    "target": "projects/123456789/locations/us-central1/jobs/test-job",
    "verb": "delete",
    "apiVersion": "v1"
  },
  "done": false
}|}
          |> Operation.of_yojson |> get_ok
        in
        Alcotest.(check bool) "not done" false op.done_;
        let meta = Operation.metadata op |> get_ok |> Option.get in
        Alcotest.(check (option string)) "verb" (Some "delete") meta.verb;
        Alcotest.(check bool)
          "requestedCancellation" false meta.requested_cancellation;
        let failed =
          Yojson.Safe.from_string
            {|{ "name": "projects/1/locations/l/operations/x", "done": true,
                "error": { "code": 5, "message": "Job not found" } }|}
          |> Operation.of_yojson |> get_ok
        in
        Alcotest.(check bool) "done" true failed.done_;
        Alcotest.(check (option int))
          "error code" (Some 5)
          (Option.bind failed.error (fun e -> e.code));
        Alcotest.(check bool)
          "no metadata" true
          (Operation.metadata failed = Ok None));
    Alcotest_lwt.test_case_sync "enums" `Quick (fun () ->
        Alcotest.(check int) "all job states" 9 (List.length Job_state.all);
        Alcotest.(check (option job_state))
          "of_string" (Some Job_state.RUNNING)
          (Job_state.of_string "RUNNING");
        Alcotest.(check string)
          "to_string" "CANCELLED"
          (Job_state.to_string Job_state.CANCELLED);
        Alcotest.(check bool)
          "unknown value is an error" true
          (Result.is_error (Job_state.of_yojson (`String "BOGUS")));
        Alcotest.(check bool)
          "unknown state fails job parse" true
          (Result.is_error
             (Job.of_yojson
                (`Assoc [ ("status", `Assoc [ ("state", `String "BOGUS") ]) ])));
        Alcotest.(check job_state)
          "missing state is UNSPECIFIED" Job_state.STATE_UNSPECIFIED
          (Job.state
             (Job.of_yojson (`Assoc [ ("status", `Assoc []) ]) |> get_ok)));
    (* Requires the Batch API to be enabled in the project *)
    Alcotest_lwt.test_case "projects.locations.jobs.list" `Quick (fun _ () ->
        let open Lwt.Infix in
        Projects.Locations.Jobs.list ~project_id:"imandra-dev"
          ~location:"us-central1" ~page_size:5 ()
        >>= function
        | Ok resp ->
            Alcotest.(check bool)
              "at most page_size jobs" true
              (List.length resp.jobs <= 5)
            |> Lwt.return
        | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e);
  ]

(* Shared types are the same across versions: a v1 Script is a v1alpha Script. *)
let (_ : Alpha.Script.t) = Script.make ~text:"echo shared" ()

let alpha_job_response =
  {|{
  "name": "projects/123456789/locations/us-central1/jobs/alpha-job",
  "taskGroups": [
    {
      "taskSpec": { "runnables": [{ "script": { "text": "echo hi" } }] },
      "taskCount": "1",
      "labels": { "tier": "alpha" },
      "serviceAccount": { "email": "sa@example.iam.gserviceaccount.com" }
    }
  ],
  "dependencies": [
    { "items": { "projects/123456789/locations/us-central1/jobs/upstream": "SUCCEEDED" } }
  ],
  "schedulingPolicy": "AS_SOON_AS_POSSIBLE",
  "status": {
    "state": "FAILED",
    "statusEvents": [
      {
        "type": "STATUS_CHANGED",
        "taskState": "FAILED",
        "taskExecution": { "exitCode": 1, "stderrSnippet": "boom" }
      }
    ],
    "resourceUsage": { "coreHours": 0.25 }
  }
}|}

let alpha_tests : unit Alcotest_lwt.test_case list =
  [
    Alcotest_lwt.test_case_sync "Job.of_yojson (alpha fields)" `Quick (fun () ->
        let job =
          Yojson.Safe.from_string alpha_job_response
          |> Alpha.Job.of_yojson |> get_ok
        in
        Alcotest.(check (option string))
          "id" (Some "alpha-job") (Alpha.Job.id job);
        Alcotest.(check job_state)
          "state" Job_state.FAILED (Alpha.Job.state job);
        let dep = List.hd job.dependencies in
        Alcotest.(check bool)
          "dependency type" true
          (dep.items
          = [
              ( "projects/123456789/locations/us-central1/jobs/upstream",
                Alpha.Job_dependency_type.SUCCEEDED );
            ]);
        let tg = List.hd job.task_groups in
        Alcotest.(check (list (pair string string)))
          "task group labels"
          [ ("tier", "alpha") ]
          tg.labels;
        Alcotest.(check (option string))
          "task group service account"
          (Some "sa@example.iam.gserviceaccount.com")
          (Option.bind tg.service_account (fun sa -> sa.email));
        let status = Option.get job.status in
        Alcotest.(check (option (float 0.001)))
          "core hours" (Some 0.25)
          (Option.bind status.resource_usage (fun r -> r.core_hours));
        let ev = List.hd status.status_events in
        Alcotest.(check (option string))
          "stderr snippet" (Some "boom")
          (Option.bind ev.task_execution (fun e -> e.stderr_snippet));
        let j = Alpha.Job.to_yojson job in
        Alcotest.(check json)
          "round-trip" j
          (Alpha.Job.of_yojson j |> get_ok |> Alpha.Job.to_yojson));
    Alcotest_lwt.test_case_sync "Job.make with dependencies" `Quick (fun () ->
        let open Yojson.Safe.Util in
        let job =
          Alpha.Job.make
            ~task_groups:
              [
                Alpha.Task_group.make
                  ~task_spec:
                    (Task_spec.make
                       ~runnables:
                         [
                           Runnable.make
                             ~script:(Script.make ~text:"true" ())
                             ();
                         ]
                       ())
                  ~allocation_policy:
                    (Alpha.Allocation_policy.make
                       ~instance_flexibility_policy:
                         (Alpha.Instance_flexibility_policy.make
                            ~instance_selections:
                              [
                                ( "small",
                                  Alpha.Instance_selection.make
                                    ~machine_types:
                                      [ "e2-standard-2"; "n2-standard-2" ]
                                    ~rank:1 () );
                              ])
                       ())
                  ();
              ]
            ~dependencies:
              [
                Alpha.Job_dependency.make
                  ~items:
                    [
                      ( "projects/p/locations/l/jobs/up",
                        Alpha.Job_dependency_type.FINISHED );
                    ];
              ]
            ()
        in
        let j = Alpha.Job.to_yojson job in
        Alcotest.(check bool) "no nulls" false (has_null j);
        Alcotest.(check json)
          "dependency items"
          (`Assoc [ ("projects/p/locations/l/jobs/up", `String "FINISHED") ])
          (member "dependencies" j |> index 0 |> member "items");
        Alcotest.(check json)
          "instance selection machine types"
          (`List [ `String "e2-standard-2"; `String "n2-standard-2" ])
          (member "taskGroups" j |> index 0 |> member "allocationPolicy"
          |> member "instanceFlexibilityPolicy"
          |> member "instanceSelections"
          |> member "small" |> member "machineTypes"));
    Alcotest_lwt.test_case_sync "Resource_allowance" `Quick (fun () ->
        let open Yojson.Safe.Util in
        let ra =
          Alpha.Resource_allowance.make
            ~usage_resource_allowance:
              (Alpha.Usage_resource_allowance.make
                 ~spec:
                   (Alpha.Usage_resource_allowance_spec.make
                      ~type_:"cpu-core-hours"
                      ~limit:
                        (Alpha.Limit.make
                           ~calendar_period:Alpha.Calendar_period.MONTH
                           ~limit:10000. ())))
            ~notifications:
              [ Alpha.Notification.make ~pubsub_topic:"projects/p/topics/t" ]
            ()
        in
        let j = Alpha.Resource_allowance.to_yojson ra in
        Alcotest.(check bool) "no nulls" false (has_null j);
        Alcotest.(check json)
          "calendar period" (`String "MONTH")
          (member "usageResourceAllowance" j
          |> member "spec" |> member "limit" |> member "calendarPeriod");
        let parsed =
          Yojson.Safe.from_string
            {|{
  "name": "projects/123/locations/us-central1/resourceAllowances/ra-1",
  "uid": "u-1",
  "usageResourceAllowance": {
    "spec": { "type": "cpu-core-hours", "limit": { "calendarPeriod": "MONTH", "limit": 10000 } },
    "status": {
      "state": "RESOURCE_ALLOWANCE_ACTIVE",
      "limitStatus": { "limit": 10000, "consumed": 12.5,
                       "consumptionInterval": { "startTime": "2025-09-01T00:00:00Z", "endTime": "2025-10-01T00:00:00Z" } },
      "report": { "latestPeriodConsumptions": { "MONTH": { "consumed": 12.5 } } }
    }
  },
  "createTime": "2025-09-01T00:00:00Z"
}|}
          |> Alpha.Resource_allowance.of_yojson |> get_ok
        in
        Alcotest.(check (option string))
          "id" (Some "ra-1")
          (Alpha.Resource_allowance.id parsed);
        let status =
          Option.get (Option.get parsed.usage_resource_allowance).status
        in
        Alcotest.(check bool)
          "state" true
          (status.state
         = Alpha.Resource_allowance_state.RESOURCE_ALLOWANCE_ACTIVE);
        Alcotest.(check (option (float 0.001)))
          "consumed" (Some 12.5)
          (Option.bind status.limit_status (fun l -> l.consumed));
        Alcotest.(check (option (float 0.001)))
          "period consumption" (Some 12.5)
          (Option.bind status.report (fun r ->
               Option.bind (List.assoc_opt "MONTH" r.latest_period_consumptions)
                 (fun p -> p.consumed))));
  ]
