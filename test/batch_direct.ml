(* Hermetic tests for the direct-style backend: the Batch functor applied to
   Gcloud_direct's async module and a canned in-memory client. *)

open Gcloud.Batch_types

let job_json ~state =
  Printf.sprintf
    {|{"name":"projects/123/locations/us-central1/jobs/j1","uid":"u",
       "taskGroups":[{"name":"projects/123/locations/us-central1/jobs/j1/taskGroups/group0",
                      "taskSpec":{"runnables":[{"script":{"text":"echo hi"}}]},"taskCount":"1"}],
       "status":{"state":"%s"}}|}
    state

(* Records requests and replies from a queue of canned responses. *)
module Fake_client = struct
  type 'a task = 'a

  let requests : (Cohttp.Code.meth * string * string option) list ref = ref []
  let responses : (Cohttp.Code.status_code * string) list ref = ref []

  let reset ~replies =
    requests := [];
    responses := replies

  let call ~meth ~headers:_ ?body uri =
    requests := (meth, Uri.to_string uri, body) :: !requests;
    match !responses with
    | r :: rest ->
        responses := rest;
        r
    | [] -> failwith "Fake_client: no canned response left"

  let get_access_token ~scopes () =
    let token =
      Gcloud.Auth.Access_token.make ~access_token:"tok" ~expires_in:3600 ()
    in
    Ok
      {
        Gcloud.Auth.credentials = GCE_metadata { project_id = "fake-project" };
        token;
        created_at = Unix.time ();
        scopes;
      }

  let get_project_id ?project_id ~token_info:_ () =
    Ok (Option.value project_id ~default:"fake-project")
end

module Batch_fake =
  Gcloud.Batch.V1.Make (Gcloud_direct.Async_task_direct) (Fake_client)

(* Real libcurl transport, fake credentials. *)
module Batch_curl =
  Gcloud.Batch.V1.Make
    (Gcloud_direct.Async_task_direct)
    (Gcloud_direct.Client_ezcurl.Make (Fake_client))

let get_ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "Error:\n%a" Gcloud.Error.pp e

let tests : unit Alcotest_lwt.test_case list =
  [
    Alcotest_lwt.test_case_sync "jobs.get through fake client" `Quick (fun () ->
        Fake_client.reset ~replies:[ (`OK, job_json ~state:"RUNNING") ];
        let job =
          Batch_fake.Projects.Locations.Jobs.get ~location:"us-central1"
            ~job:"j1" ()
          |> get_ok
        in
        Alcotest.(check string)
          "job id" "j1"
          (Option.get (Gcloud.Batch.V1.Job.id job));
        Alcotest.(check bool)
          "state" true
          (Gcloud.Batch.V1.Job.state job = Job_state.RUNNING);
        match !Fake_client.requests with
        | [ (`GET, url, None) ] ->
            Alcotest.(check string)
              "url"
              "https://batch.googleapis.com/v1/projects/fake-project/locations/us-central1/jobs/j1"
              url
        | _ -> Alcotest.fail "expected exactly one GET without body");
    Alcotest_lwt.test_case_sync "jobs.create sends JSON body" `Quick (fun () ->
        Fake_client.reset ~replies:[ (`OK, job_json ~state:"QUEUED") ];
        let job =
          Gcloud.Batch.V1.Job.make
            ~task_groups:
              [
                Gcloud.Batch.V1.Task_group.make
                  ~task_spec:
                    (Task_spec.make
                       ~runnables:
                         [
                           Runnable.make
                             ~script:(Script.make ~text:"true" ())
                             ();
                         ]
                       ())
                  ();
              ]
            ()
        in
        let _ =
          Batch_fake.Projects.Locations.Jobs.create ~project_id:"p"
            ~location:"l" ~job_id:"my-job" job
          |> get_ok
        in
        match !Fake_client.requests with
        | [ (`POST, url, Some body) ] ->
            Alcotest.(check string)
              "url"
              "https://batch.googleapis.com/v1/projects/p/locations/l/jobs?jobId=my-job"
              url;
            let sent = Yojson.Safe.from_string body in
            Alcotest.(check bool)
              "body is the job" true
              (sent = Gcloud.Batch.V1.Job.to_yojson job)
        | _ -> Alcotest.fail "expected exactly one POST with body");
    Alcotest_lwt.test_case_sync
      "poll_until_complete sleeps and returns terminal job" `Quick (fun () ->
        Fake_client.reset
          ~replies:
            [
              (`OK, job_json ~state:"QUEUED");
              (`OK, job_json ~state:"RUNNING");
              (`OK, job_json ~state:"SUCCEEDED");
            ];
        let t0 = Unix.gettimeofday () in
        let job =
          Batch_fake.Projects.Locations.Jobs.poll_until_complete ~location:"l"
            ~job:"j1" ~poll_every_s:0.05 ()
          |> get_ok
        in
        Alcotest.(check bool)
          "succeeded" true
          (Gcloud.Batch.V1.Job.state job = Job_state.SUCCEEDED);
        Alcotest.(check int) "three polls" 3 (List.length !Fake_client.requests);
        Alcotest.(check bool)
          "slept twice" true
          (Unix.gettimeofday () -. t0 >= 0.1));
    Alcotest_lwt.test_case_sync "API error status is reported" `Quick (fun () ->
        Fake_client.reset
          ~replies:[ (`Not_found, {|{"error":{"code":404,"message":"nope"}}|}) ];
        match
          Batch_fake.Projects.Locations.Jobs.get ~location:"l" ~job:"missing" ()
        with
        | Error (`Gcloud_api_error (`Not_found, Gcloud.Error.Json { error })) ->
            Alcotest.(check string) "message" "nope" error.message
        | Error e -> Alcotest.failf "unexpected error: %a" Gcloud.Error.pp e
        | Ok _ -> Alcotest.fail "expected an error");
    Alcotest_lwt.test_case_sync
      "libcurl transport failure becomes Network_error" `Quick (fun () ->
        (* A closed local port exercises the transport hermetically. *)
        let raised =
          try
            ignore
              (Gcloud_direct.Http_ezcurl.call ~timeout_s:2. ~meth:`GET
                 ~headers:(Cohttp.Header.init ())
                 (Uri.of_string "http://127.0.0.1:9/"));
            false
          with Gcloud_direct.Http_ezcurl.Curl_error _ -> true
        in
        Alcotest.(check bool) "Curl_error raised" true raised;
        let via_functor =
          (* Over the real transport: Network_error offline, or an API error
             for the fake token online. Either proves the pipeline end to end. *)
          match
            Batch_curl.Projects.Locations.Jobs.get ~location:"l" ~job:"j" ()
          with
          | Error (`Network_error _) | Error (`Gcloud_api_error _) -> true
          | Error e -> Alcotest.failf "unexpected error: %a" Gcloud.Error.pp e
          | Ok _ -> false
        in
        Alcotest.(check bool) "functor over real transport" true via_functor);
    Alcotest_lwt.test_case_sync "credentials from env JSON" `Quick (fun () ->
        Unix.putenv
          Gcloud.Auth.Environment_vars.google_application_credentials_json
          {|{"type":"service_account","client_email":"sa@p.iam.gserviceaccount.com",
             "private_key":"-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n",
             "project_id":"p","token_uri":"https://oauth2.googleapis.com/token"}|};
        (match
           Gcloud_direct.Auth.discover_credentials_with
             Discover_credentials_json_from_env
         with
        | Ok (Gcloud.Auth.Service_account c) ->
            Alcotest.(check string) "project" "p" c.project_id
        | Ok _ -> Alcotest.fail "expected service account"
        | Error e -> Alcotest.failf "Error: %a" Gcloud.Auth.pp_error e);
        Unix.putenv
          Gcloud.Auth.Environment_vars.google_application_credentials_json "");
    Alcotest_lwt.test_case_sync "impersonated access token parses" `Quick
      (fun () ->
        let expire_time =
          Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600)
          |> Option.get
          |> Ptime.to_rfc3339 ~tz_offset_s:0
        in
        let json =
          `Assoc
            [
              ("accessToken", `String "tok"); ("expireTime", `String expire_time);
            ]
        in
        match Gcloud.Auth.impersonated_access_token_of_json json with
        | Ok t ->
            Alcotest.(check string) "token" "tok" t.access_token;
            Alcotest.(check bool)
              "expires_in ~1h" true
              (t.expires_in >= 3590 && t.expires_in <= 3600);
            Alcotest.(check (list string))
              "refresh scopes" [ Gcloud.Auth.Scopes.iam ]
              t.additional_refresh_scopes
        | Error (`Bad_token_response msg) -> Alcotest.failf "Error: %s" msg);
  ]
