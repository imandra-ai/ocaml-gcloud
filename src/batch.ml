(** Bindings to the Cloud Batch API.

    https://cloud.google.com/batch/docs/reference/rest *)

module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
end

let host = "batch.googleapis.com"

(* {1 Request plumbing} *)

let query_opt (name : string) (value : string option) :
    (string * string list) list =
  value |> CCOption.map_or ~default:[] (fun v -> [ (name, [ v ]) ])

module Plumbing
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  module R = Async_task_result.Make (Async)
  module Req = Request.Make (Async) (Client)

  let call_with_token ~(token_info : Auth.token_info) ~(meth : Cohttp.Code.meth)
      ?(query = []) ?(body : Yojson.Safe.t option) ~(path : string)
      (parse : Yojson.Safe.t -> ('a, string) result) :
      ('a, [> Error.t ]) result Async.t =
    let open R.Infix in
    let uri = Uri.make () ~scheme:"https" ~host ~path ~query in
    let headers =
      Cohttp.Header.of_list
        (Req.bearer token_info
        ::
        (match body with
        | Some _ -> [ ("Content-Type", "application/json") ]
        | None -> []))
    in
    let body = CCOption.map Yojson.Safe.to_string body in
    Req.call ~meth ~headers ?body uri >>= fun (status, body) ->
    match status with
    | `OK -> R.lift (Error.parse_body_json parse body)
    | status_code ->
        R.lift (Error.of_response_status_code_and_body status_code body)

  (** Call an endpoint addressed by a full resource name (no project lookup). *)
  let call ~meth ?query ?body ~path parse =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.cloud_platform ] ()
    >>= fun token_info ->
    call_with_token ~token_info ~meth ?query ?body ~path parse

  (** Call an endpoint whose path depends on the resolved project ID. *)
  let call_in_project ?project_id ~meth ?query ?body
      ~(path : project_id:string -> string) parse =
    let open R.Infix in
    Client.get_access_token ~scopes:[ Scopes.cloud_platform ] ()
    >>= fun token_info ->
    Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
    call_with_token ~token_info ~meth ?query ?body ~path:(path ~project_id)
      parse
end

(* {1 API methods shared between versions} *)

module type VERSION = sig
  val version : string

  module Job : sig
    type t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val state : t -> Batch_types.Job_state.t
  end

  module Task : sig
    type t

    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_jobs_response : sig
    type t

    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_tasks_response : sig
    type t

    val of_yojson : Yojson.Safe.t -> (t, string) result
  end
end

module Make_api
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t)
    (X : VERSION) =
struct
  include Plumbing (Async) (Client)

  let path ~project_id ~location =
    Printf.sprintf "%s/projects/%s/locations/%s" X.version project_id location

  module Jobs = struct
    let create ?project_id ~location ?job_id ?request_id (job : X.Job.t) :
        (X.Job.t, [> Error.t ]) result Async.t =
      let query =
        List.concat
          [ query_opt "jobId" job_id; query_opt "requestId" request_id ]
      in
      call_in_project ?project_id ~meth:`POST ~query ~body:(X.Job.to_yojson job)
        ~path:(fun ~project_id -> path ~project_id ~location ^ "/jobs")
        X.Job.of_yojson

    let get ?project_id ~location ~job () :
        (X.Job.t, [> Error.t ]) result Async.t =
      call_in_project ?project_id ~meth:`GET
        ~path:(fun ~project_id ->
          Printf.sprintf "%s/jobs/%s" (path ~project_id ~location) job)
        X.Job.of_yojson

    let list ?project_id ~location ?filter ?order_by ?page_size ?page_token () :
        (X.List_jobs_response.t, [> Error.t ]) result Async.t =
      let query =
        List.concat
          [
            query_opt "filter" filter;
            query_opt "orderBy" order_by;
            query_opt "pageSize" (CCOption.map string_of_int page_size);
            query_opt "pageToken" page_token;
          ]
      in
      call_in_project ?project_id ~meth:`GET ~query
        ~path:(fun ~project_id -> path ~project_id ~location ^ "/jobs")
        X.List_jobs_response.of_yojson

    let delete ?project_id ~location ?reason ?request_id ~job () :
        (Batch_types.Operation.t, [> Error.t ]) result Async.t =
      let query =
        List.concat
          [ query_opt "reason" reason; query_opt "requestId" request_id ]
      in
      call_in_project ?project_id ~meth:`DELETE ~query
        ~path:(fun ~project_id ->
          Printf.sprintf "%s/jobs/%s" (path ~project_id ~location) job)
        Batch_types.Operation.of_yojson

    let cancel ?project_id ~location ?request_id ~job () :
        (Batch_types.Operation.t, [> Error.t ]) result Async.t =
      let body =
        `Assoc
          (request_id
          |> CCOption.map_or ~default:[] (fun r -> [ ("requestId", `String r) ])
          )
      in
      call_in_project ?project_id ~meth:`POST ~body
        ~path:(fun ~project_id ->
          Printf.sprintf "%s/jobs/%s:cancel" (path ~project_id ~location) job)
        Batch_types.Operation.of_yojson

    let poll_until_complete ?project_id ~location ?(poll_every_s = 10.)
        ?timeout_s ~job () : (X.Job.t, [> Error.t ]) result Async.t =
      let open R.Infix in
      let deadline =
        timeout_s |> CCOption.map (fun t -> Unix.gettimeofday () +. t)
      in
      let rec loop () =
        get ?project_id ~location ~job () >>= fun j ->
        let state = X.Job.state j in
        if Batch_types.Job_state.is_terminal state then R.return j
        else
          match deadline with
          | Some d when Unix.gettimeofday () >= d ->
              R.fail
                (`Gcloud_retry_timeout
                  (Printf.sprintf
                     "Batch.%s.Projects.Locations.Jobs.poll_until_complete: \
                      job %s still %s after %.0fs"
                     X.version job
                     (Batch_types.Job_state.show state)
                     (CCOption.get_or ~default:0. timeout_s)))
          | _ -> R.ok (Async.sleep poll_every_s) >>= loop
      in
      loop ()
  end

  module Tasks = struct
    let get ?project_id ~location ~job ?(task_group = "group0") ~task () :
        (X.Task.t, [> Error.t ]) result Async.t =
      call_in_project ?project_id ~meth:`GET
        ~path:(fun ~project_id ->
          Printf.sprintf "%s/jobs/%s/taskGroups/%s/tasks/%s"
            (path ~project_id ~location)
            job task_group task)
        X.Task.of_yojson

    let list ?project_id ~location ~job ?(task_group = "group0") ?filter
        ?order_by ?page_size ?page_token () :
        (X.List_tasks_response.t, [> Error.t ]) result Async.t =
      let query =
        List.concat
          [
            query_opt "filter" filter;
            query_opt "orderBy" order_by;
            query_opt "pageSize" (CCOption.map string_of_int page_size);
            query_opt "pageToken" page_token;
          ]
      in
      call_in_project ?project_id ~meth:`GET ~query
        ~path:(fun ~project_id ->
          Printf.sprintf "%s/jobs/%s/taskGroups/%s/tasks"
            (path ~project_id ~location)
            job task_group)
        X.List_tasks_response.of_yojson
  end

  module Operations = struct
    let get ~name () : (Batch_types.Operation.t, [> Error.t ]) result Async.t =
      call ~meth:`GET
        ~path:(Printf.sprintf "%s/%s" X.version name)
        Batch_types.Operation.of_yojson

    let list ?project_id ~location ?filter ?page_size ?page_token () :
        (Batch_types.List_operations_response.t, [> Error.t ]) result Async.t =
      let query =
        List.concat
          [
            query_opt "filter" filter;
            query_opt "pageSize" (CCOption.map string_of_int page_size);
            query_opt "pageToken" page_token;
          ]
      in
      call_in_project ?project_id ~meth:`GET ~query
        ~path:(fun ~project_id -> path ~project_id ~location ^ "/operations")
        Batch_types.List_operations_response.of_yojson

    let cancel ~name () : (unit, [> Error.t ]) result Async.t =
      call ~meth:`POST ~body:(`Assoc [])
        ~path:(Printf.sprintf "%s/%s:cancel" X.version name) (fun _ -> Ok ())

    let delete ~name () : (unit, [> Error.t ]) result Async.t =
      call ~meth:`DELETE ~path:(Printf.sprintf "%s/%s" X.version name) (fun _ ->
          Ok ())
  end
end

let id_of_name (name : string) : string =
  match String.rindex_opt name '/' with
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)
  | None -> name

(* {1 v1} *)

module V1 = struct
  include Batch_types

  [@@@warning "-39"]

  module Task_group = struct
    type t = {
      name : string option; [@yojson.default None]
      task_spec : Task_spec.t; [@key "taskSpec"]
      task_count : Int64_string.t option;
          [@key "taskCount"] [@yojson.default None]
      parallelism : Int64_string.t option; [@yojson.default None]
      scheduling_policy : Scheduling_policy.t option;
          [@key "schedulingPolicy"] [@yojson.default None]
      task_environments : Environment.t list;
          [@key "taskEnvironments"] [@default []]
      task_count_per_node : Int64_string.t option;
          [@key "taskCountPerNode"] [@yojson.default None]
      require_hosts_file : bool; [@key "requireHostsFile"] [@default false]
      permissive_ssh : bool; [@key "permissiveSsh"] [@default false]
      run_as_non_root : bool; [@key "runAsNonRoot"] [@default false]
    }
    [@@deriving yojson { strict = false }]

    let make ~task_spec ?task_count ?parallelism ?scheduling_policy
        ?(task_environments = []) ?task_count_per_node
        ?(require_hosts_file = false) ?(permissive_ssh = false)
        ?(run_as_non_root = false) () =
      {
        name = None;
        task_spec;
        task_count;
        parallelism;
        scheduling_policy;
        task_environments;
        task_count_per_node;
        require_hosts_file;
        permissive_ssh;
        run_as_non_root;
      }
  end

  module Allocation_policy = struct
    type t = {
      location : Location_policy.t option; [@yojson.default None]
      instances : Instance_policy_or_template.t list; [@default []]
      service_account : Service_account.t option;
          [@key "serviceAccount"] [@yojson.default None]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      network : Network_policy.t option; [@yojson.default None]
      placement : Placement_policy.t option; [@yojson.default None]
      tags : string list; [@default []]
    }
    [@@deriving yojson { strict = false }, make]
  end

  module Task_execution = struct
    type t = { exit_code : int option [@key "exitCode"] [@yojson.default None] }
    [@@deriving yojson { strict = false }]
  end

  module Status_event = struct
    type t = {
      type_ : string option; [@key "type"] [@yojson.default None]
      description : string option; [@yojson.default None]
      event_time : string option; [@key "eventTime"] [@yojson.default None]
      task_execution : Task_execution.t option;
          [@key "taskExecution"] [@yojson.default None]
      task_state : Task_state.t option; [@key "taskState"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Job_status = struct
    type t = {
      state : Job_state.t; [@default Job_state.STATE_UNSPECIFIED]
      status_events : Status_event.t list; [@key "statusEvents"] [@default []]
      task_groups : (string * Task_group_status.t) list;
          [@key "taskGroups"]
          [@default []]
          [@to_yojson Task_group_status.map_to_yojson]
          [@of_yojson Task_group_status.map_of_yojson]
      run_duration : string option; [@key "runDuration"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Task_status = struct
    type t = {
      state : Task_state.t; [@default Task_state.STATE_UNSPECIFIED]
      status_events : Status_event.t list; [@key "statusEvents"] [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  module Task = struct
    type t = {
      name : string;
      status : Task_status.t option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]

    let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)
  end

  module Job = struct
    type t = {
      name : string option; [@yojson.default None]
      uid : string option; [@yojson.default None]
      priority : Int64_string.t option; [@yojson.default None]
      task_groups : Task_group.t list; [@key "taskGroups"] [@default []]
      allocation_policy : Allocation_policy.t option;
          [@key "allocationPolicy"] [@yojson.default None]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      status : Job_status.t option; [@yojson.default None]
      create_time : string option; [@key "createTime"] [@yojson.default None]
      update_time : string option; [@key "updateTime"] [@yojson.default None]
      logs_policy : Logs_policy.t option;
          [@key "logsPolicy"] [@yojson.default None]
      notifications : Job_notification.t list; [@default []]
    }
    [@@deriving yojson { strict = false }]

    let make ~task_groups ?priority ?allocation_policy ?(labels = [])
        ?logs_policy ?(notifications = []) () =
      {
        name = None;
        uid = None;
        priority;
        task_groups;
        allocation_policy;
        labels;
        status = None;
        create_time = None;
        update_time = None;
        logs_policy;
        notifications;
      }

    let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)
    let id (t : t) : string option = CCOption.map id_of_name t.name

    let state (t : t) : Job_state.t =
      match t.status with
      | Some { state; _ } -> state
      | None -> Job_state.STATE_UNSPECIFIED
  end

  module List_jobs_response = struct
    type t = {
      jobs : Job.t list; [@default []]
      next_page_token : string option;
          [@key "nextPageToken"] [@yojson.default None]
      unreachable : string list; [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  module List_tasks_response = struct
    type t = {
      tasks : Task.t list; [@default []]
      next_page_token : string option;
          [@key "nextPageToken"] [@yojson.default None]
      unreachable : string list; [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  [@@@warning "+39"]

  module Make
      (Async : Async_task_sig.S)
      (Client : Client_sig.S with type 'a task = 'a Async.t) =
  struct
    type 'a task = 'a Async.t

    module Projects = struct
      module Locations = struct
        module Api =
          Make_api (Async) (Client)
            (struct
              let version = "v1"

              module Job = Job
              module Task = Task
              module List_jobs_response = List_jobs_response
              module List_tasks_response = List_tasks_response
            end)

        module Jobs = struct
          include Api.Jobs

          module TaskGroups = struct
            module Tasks = struct
              let get = Api.Tasks.get

              (* [orderBy] is only supported by v1alpha. *)
              let list ?project_id ~location ~job ?task_group ?filter ?page_size
                  ?page_token () =
                Api.Tasks.list ?project_id ~location ~job ?task_group ?filter
                  ?page_size ?page_token ()
            end
          end
        end

        module Operations = Api.Operations
      end
    end
  end
end

(* {1 v1alpha} *)

module V1alpha = struct
  include Batch_types

  module Job_dependency_type = struct
    module T = struct
      type t = TYPE_UNSPECIFIED | SUCCEEDED | FAILED | FINISHED
      [@@deriving show { with_path = false }, enum]
    end

    include T
    include Enum.Make (T)
  end

  module Calendar_period = struct
    module T = struct
      type t =
        | CALENDAR_PERIOD_UNSPECIFIED
        | MONTH
        | QUARTER
        | YEAR
        | WEEK
        | DAY
      [@@deriving show { with_path = false }, enum]
    end

    include T
    include Enum.Make (T)
  end

  module Resource_allowance_state = struct
    module T = struct
      type t =
        | RESOURCE_ALLOWANCE_STATE_UNSPECIFIED
        | RESOURCE_ALLOWANCE_ACTIVE
        | RESOURCE_ALLOWANCE_DEPLETED
      [@@deriving show { with_path = false }, enum]
    end

    include T
    include Enum.Make (T)
  end

  [@@@warning "-39"]

  module Job_dependency = struct
    let items_to_yojson = Assoc.to_yojson Job_dependency_type.to_yojson
    let items_of_yojson = Assoc.of_yojson Job_dependency_type.of_yojson

    type t = {
      items : (string * Job_dependency_type.t) list;
          [@default []]
          [@to_yojson items_to_yojson]
          [@of_yojson items_of_yojson]
    }
    [@@deriving yojson { strict = false }]

    let make ~items = { items }
  end

  module Instance_selection = struct
    type t = {
      rank : int option; [@yojson.default None]
      boot_disk : Disk.t option; [@key "bootDisk"] [@yojson.default None]
      disks : Attached_disk.t list; [@default []]
      machine_types : string list; [@key "machineTypes"] [@default []]
    }
    [@@deriving yojson { strict = false }]

    let make ~machine_types ?rank ?boot_disk ?(disks = []) () =
      { rank; boot_disk; disks; machine_types }
  end

  module Instance_flexibility_policy = struct
    let selections_to_yojson = Assoc.to_yojson Instance_selection.to_yojson
    let selections_of_yojson = Assoc.of_yojson Instance_selection.of_yojson

    type t = {
      instance_selections : (string * Instance_selection.t) list;
          [@key "instanceSelections"]
          [@default []]
          [@to_yojson selections_to_yojson]
          [@of_yojson selections_of_yojson]
    }
    [@@deriving yojson { strict = false }]

    let make ~instance_selections = { instance_selections }
  end

  module Allocation_policy = struct
    type t = {
      location : Location_policy.t option; [@yojson.default None]
      instances : Instance_policy_or_template.t list; [@default []]
      instance_flexibility_policy : Instance_flexibility_policy.t option;
          [@key "instanceFlexibilityPolicy"] [@yojson.default None]
      service_account : Service_account.t option;
          [@key "serviceAccount"] [@yojson.default None]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      network : Network_policy.t option; [@yojson.default None]
      placement : Placement_policy.t option; [@yojson.default None]
      tags : string list; [@default []]
    }
    [@@deriving yojson { strict = false }, make]
  end

  module Task_group = struct
    type t = {
      name : string option; [@yojson.default None]
      task_spec : Task_spec.t; [@key "taskSpec"]
      task_count : Int64_string.t option;
          [@key "taskCount"] [@yojson.default None]
      parallelism : Int64_string.t option; [@yojson.default None]
      scheduling_policy : Scheduling_policy.t option;
          [@key "schedulingPolicy"] [@yojson.default None]
      allocation_policy : Allocation_policy.t option;
          [@key "allocationPolicy"] [@yojson.default None]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      task_environments : Environment.t list;
          [@key "taskEnvironments"] [@default []]
      task_count_per_node : Int64_string.t option;
          [@key "taskCountPerNode"] [@yojson.default None]
      require_hosts_file : bool; [@key "requireHostsFile"] [@default false]
      permissive_ssh : bool; [@key "permissiveSsh"] [@default false]
      run_as_non_root : bool; [@key "runAsNonRoot"] [@default false]
      service_account : Service_account.t option;
          [@key "serviceAccount"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]

    let make ~task_spec ?task_count ?parallelism ?scheduling_policy
        ?allocation_policy ?(labels = []) ?(task_environments = [])
        ?task_count_per_node ?(require_hosts_file = false)
        ?(permissive_ssh = false) ?(run_as_non_root = false) ?service_account ()
        =
      {
        name = None;
        task_spec;
        task_count;
        parallelism;
        scheduling_policy;
        allocation_policy;
        labels;
        task_environments;
        task_count_per_node;
        require_hosts_file;
        permissive_ssh;
        run_as_non_root;
        service_account;
      }
  end

  module Resource_usage = struct
    type t = {
      core_hours : float option; [@key "coreHours"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Task_execution = struct
    type t = {
      exit_code : int option; [@key "exitCode"] [@yojson.default None]
      stderr_snippet : string option;
          [@key "stderrSnippet"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Status_event = struct
    type t = {
      type_ : string option; [@key "type"] [@yojson.default None]
      description : string option; [@yojson.default None]
      event_time : string option; [@key "eventTime"] [@yojson.default None]
      task_execution : Task_execution.t option;
          [@key "taskExecution"] [@yojson.default None]
      task_state : Task_state.t option; [@key "taskState"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Job_status = struct
    type t = {
      state : Job_state.t; [@default Job_state.STATE_UNSPECIFIED]
      status_events : Status_event.t list; [@key "statusEvents"] [@default []]
      task_groups : (string * Task_group_status.t) list;
          [@key "taskGroups"]
          [@default []]
          [@to_yojson Task_group_status.map_to_yojson]
          [@of_yojson Task_group_status.map_of_yojson]
      run_duration : string option; [@key "runDuration"] [@yojson.default None]
      resource_usage : Resource_usage.t option;
          [@key "resourceUsage"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Task_status = struct
    type t = {
      state : Task_state.t; [@default Task_state.STATE_UNSPECIFIED]
      status_events : Status_event.t list; [@key "statusEvents"] [@default []]
      resource_usage : Resource_usage.t option;
          [@key "resourceUsage"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Task = struct
    type t = {
      name : string;
      status : Task_status.t option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]

    let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)
  end

  module Job = struct
    type t = {
      name : string option; [@yojson.default None]
      uid : string option; [@yojson.default None]
      priority : Int64_string.t option; [@yojson.default None]
      task_groups : Task_group.t list; [@key "taskGroups"] [@default []]
      scheduling_policy : Scheduling_policy.t option;
          [@key "schedulingPolicy"] [@yojson.default None]
      dependencies : Job_dependency.t list; [@default []]
      allocation_policy : Allocation_policy.t option;
          [@key "allocationPolicy"] [@yojson.default None]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      status : Job_status.t option; [@yojson.default None]
      create_time : string option; [@key "createTime"] [@yojson.default None]
      update_time : string option; [@key "updateTime"] [@yojson.default None]
      logs_policy : Logs_policy.t option;
          [@key "logsPolicy"] [@yojson.default None]
      notifications : Job_notification.t list; [@default []]
    }
    [@@deriving yojson { strict = false }]

    let make ~task_groups ?priority ?scheduling_policy ?(dependencies = [])
        ?allocation_policy ?(labels = []) ?logs_policy ?(notifications = []) ()
        =
      {
        name = None;
        uid = None;
        priority;
        task_groups;
        scheduling_policy;
        dependencies;
        allocation_policy;
        labels;
        status = None;
        create_time = None;
        update_time = None;
        logs_policy;
        notifications;
      }

    let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)
    let id (t : t) : string option = CCOption.map id_of_name t.name

    let state (t : t) : Job_state.t =
      match t.status with
      | Some { state; _ } -> state
      | None -> Job_state.STATE_UNSPECIFIED
  end

  module List_jobs_response = struct
    type t = {
      jobs : Job.t list; [@default []]
      next_page_token : string option;
          [@key "nextPageToken"] [@yojson.default None]
      unreachable : string list; [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  module List_tasks_response = struct
    type t = {
      tasks : Task.t list; [@default []]
      next_page_token : string option;
          [@key "nextPageToken"] [@yojson.default None]
      unreachable : string list; [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  (* Resource allowances *)

  module Interval = struct
    type t = {
      start_time : string option; [@key "startTime"] [@yojson.default None]
      end_time : string option; [@key "endTime"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }, make]
  end

  module Limit = struct
    type t = {
      calendar_period : Calendar_period.t option;
          [@key "calendarPeriod"] [@yojson.default None]
      limit : float option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }, make]
  end

  module Limit_status = struct
    type t = {
      consumption_interval : Interval.t option;
          [@key "consumptionInterval"] [@yojson.default None]
      limit : float option; [@yojson.default None]
      consumed : float option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Period_consumption = struct
    type t = {
      consumption_interval : Interval.t option;
          [@key "consumptionInterval"] [@yojson.default None]
      consumed : float option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Consumption_report = struct
    let map_to_yojson = Assoc.to_yojson Period_consumption.to_yojson
    let map_of_yojson = Assoc.of_yojson Period_consumption.of_yojson

    type t = {
      latest_period_consumptions : (string * Period_consumption.t) list;
          [@key "latestPeriodConsumptions"]
          [@default []]
          [@to_yojson map_to_yojson]
          [@of_yojson map_of_yojson]
    }
    [@@deriving yojson { strict = false }]
  end

  module Usage_resource_allowance_status = struct
    type t = {
      state : Resource_allowance_state.t;
          [@default
            Resource_allowance_state.RESOURCE_ALLOWANCE_STATE_UNSPECIFIED]
      report : Consumption_report.t option; [@yojson.default None]
      limit_status : Limit_status.t option;
          [@key "limitStatus"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Usage_resource_allowance_spec = struct
    type t = { type_ : string; [@key "type"] limit : Limit.t }
    [@@deriving yojson { strict = false }, make]
  end

  module Usage_resource_allowance = struct
    type t = {
      spec : Usage_resource_allowance_spec.t;
      status : Usage_resource_allowance_status.t option; [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]

    let make ~spec = { spec; status = None }
  end

  module Notification = struct
    type t = { pubsub_topic : string [@key "pubsubTopic"] }
    [@@deriving yojson { strict = false }, make]
  end

  module Resource_allowance = struct
    type t = {
      name : string option; [@yojson.default None]
      uid : string option; [@yojson.default None]
      usage_resource_allowance : Usage_resource_allowance.t option;
          [@key "usageResourceAllowance"] [@yojson.default None]
      notifications : Notification.t list; [@default []]
      labels : (string * string) list;
          [@default []]
          [@to_yojson String_map.to_yojson]
          [@of_yojson String_map.of_yojson]
      create_time : string option; [@key "createTime"] [@yojson.default None]
    }
    [@@deriving yojson { strict = false }]

    let make ?name ?usage_resource_allowance ?(notifications = [])
        ?(labels = []) () =
      {
        name;
        uid = None;
        usage_resource_allowance;
        notifications;
        labels;
        create_time = None;
      }

    let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)
    let id (t : t) : string option = CCOption.map id_of_name t.name
  end

  module List_resource_allowances_response = struct
    type t = {
      resource_allowances : Resource_allowance.t list;
          [@key "resourceAllowances"] [@default []]
      next_page_token : string option;
          [@key "nextPageToken"] [@yojson.default None]
      unreachable : string list; [@default []]
    }
    [@@deriving yojson { strict = false }]
  end

  [@@@warning "+39"]

  module Make
      (Async : Async_task_sig.S)
      (Client : Client_sig.S with type 'a task = 'a Async.t) =
  struct
    type 'a task = 'a Async.t

    module Projects = struct
      module Locations = struct
        module Api =
          Make_api (Async) (Client)
            (struct
              let version = "v1alpha"

              module Job = Job
              module Task = Task
              module List_jobs_response = List_jobs_response
              module List_tasks_response = List_tasks_response
            end)

        module Jobs = struct
          include Api.Jobs

          let patch ?project_id ~location ?request_id ~update_mask ~job
              (body : Job.t) : (Job.t, [> Error.t ]) result Async.t =
            let query =
              List.concat
                [
                  [ ("updateMask", [ update_mask ]) ];
                  query_opt "requestId" request_id;
                ]
            in
            Api.call_in_project ?project_id ~meth:`PATCH ~query
              ~body:(Job.to_yojson body)
              ~path:(fun ~project_id ->
                Printf.sprintf "%s/jobs/%s" (Api.path ~project_id ~location) job)
              Job.of_yojson

          module TaskGroups = struct
            module Tasks = Api.Tasks
          end
        end

        module Operations = Api.Operations

        module ResourceAllowances = struct
          let create ?project_id ~location ?resource_allowance_id ?request_id
              (resource_allowance : Resource_allowance.t) :
              (Resource_allowance.t, [> Error.t ]) result Async.t =
            let query =
              List.concat
                [
                  query_opt "resourceAllowanceId" resource_allowance_id;
                  query_opt "requestId" request_id;
                ]
            in
            Api.call_in_project ?project_id ~meth:`POST ~query
              ~body:(Resource_allowance.to_yojson resource_allowance)
              ~path:(fun ~project_id ->
                Api.path ~project_id ~location ^ "/resourceAllowances")
              Resource_allowance.of_yojson

          let get ?project_id ~location ~resource_allowance () :
              (Resource_allowance.t, [> Error.t ]) result Async.t =
            Api.call_in_project ?project_id ~meth:`GET
              ~path:(fun ~project_id ->
                Printf.sprintf "%s/resourceAllowances/%s"
                  (Api.path ~project_id ~location)
                  resource_allowance)
              Resource_allowance.of_yojson

          let list ?project_id ~location ?page_size ?page_token () :
              (List_resource_allowances_response.t, [> Error.t ]) result Async.t
              =
            let query =
              List.concat
                [
                  query_opt "pageSize" (CCOption.map string_of_int page_size);
                  query_opt "pageToken" page_token;
                ]
            in
            Api.call_in_project ?project_id ~meth:`GET ~query
              ~path:(fun ~project_id ->
                Api.path ~project_id ~location ^ "/resourceAllowances")
              List_resource_allowances_response.of_yojson

          let delete ?project_id ~location ?reason ?request_id
              ~resource_allowance () :
              (Operation.t, [> Error.t ]) result Async.t =
            let query =
              List.concat
                [ query_opt "reason" reason; query_opt "requestId" request_id ]
            in
            Api.call_in_project ?project_id ~meth:`DELETE ~query
              ~path:(fun ~project_id ->
                Printf.sprintf "%s/resourceAllowances/%s"
                  (Api.path ~project_id ~location)
                  resource_allowance)
              Operation.of_yojson

          let patch ?project_id ~location ?request_id ~update_mask
              ~resource_allowance (body : Resource_allowance.t) :
              (Resource_allowance.t, [> Error.t ]) result Async.t =
            let query =
              List.concat
                [
                  [ ("updateMask", [ update_mask ]) ];
                  query_opt "requestId" request_id;
                ]
            in
            Api.call_in_project ?project_id ~meth:`PATCH ~query
              ~body:(Resource_allowance.to_yojson body)
              ~path:(fun ~project_id ->
                Printf.sprintf "%s/resourceAllowances/%s"
                  (Api.path ~project_id ~location)
                  resource_allowance)
              Resource_allowance.of_yojson
        end
      end
    end
  end
end
