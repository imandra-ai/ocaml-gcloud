(** Bindings to the Cloud Batch API.

    Cloud Batch runs batch workloads (scripts or containers) on managed Compute
    Engine VMs. The main workflow is: build a {!V1.Job.t} with the [make]
    constructors, submit it with {!V1.Projects.Locations.Jobs.create}, then poll
    it with {!V1.Projects.Locations.Jobs.poll_until_complete}.

    {!V1} is the stable API. {!V1alpha} exposes the same surface plus
    alpha-only features: job dependencies, per-task-group allocation policies
    and service accounts, instance flexibility policies, resource usage
    reporting, [jobs.patch], and resource allowances.

    Types common to both versions live in {!Batch_types} and are re-exported
    by each version module.

    https://cloud.google.com/batch/docs/reference/rest *)

module Scopes : sig
  val cloud_platform : string
end

(** {1 v1} *)

module V1 : sig
  include module type of struct
    include Batch_types
  end

  module Task_group : sig
    type t = {
      name : string option;  (** Output only. *)
      task_spec : Task_spec.t;
      task_count : int option;
      parallelism : int option;
      scheduling_policy : Scheduling_policy.t option;
      task_environments : Environment.t list;
      task_count_per_node : int option;
      require_hosts_file : bool;
      permissive_ssh : bool;
      run_as_non_root : bool;
    }

    val make :
      task_spec:Task_spec.t ->
      ?task_count:int ->
      ?parallelism:int ->
      ?scheduling_policy:Scheduling_policy.t ->
      ?task_environments:Environment.t list ->
      ?task_count_per_node:int ->
      ?require_hosts_file:bool ->
      ?permissive_ssh:bool ->
      ?run_as_non_root:bool ->
      unit ->
      t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Allocation_policy : sig
    type t = {
      location : Location_policy.t option;
      instances : Instance_policy_or_template.t list;
      service_account : Service_account.t option;
      labels : (string * string) list;
      network : Network_policy.t option;
      placement : Placement_policy.t option;
      tags : string list;
    }

    val make :
      ?location:Location_policy.t ->
      ?instances:Instance_policy_or_template.t list ->
      ?service_account:Service_account.t ->
      ?labels:(string * string) list ->
      ?network:Network_policy.t ->
      ?placement:Placement_policy.t ->
      ?tags:string list ->
      unit ->
      t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 Status (output only)} *)

  module Task_execution : sig
    type t = { exit_code : int option }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Status_event : sig
    type t = {
      type_ : string option;
      description : string option;
      event_time : string option;
      task_execution : Task_execution.t option;
      task_state : Task_state.t option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Job_status : sig
    type t = {
      state : Job_state.t;
      status_events : Status_event.t list;
      task_groups : (string * Task_group_status.t) list;
          (** Keyed by task group ID, e.g. ["group0"]. *)
      run_duration : string option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task_status : sig
    type t = { state : Task_state.t; status_events : Status_event.t list }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task : sig
    type t = { name : string; status : Task_status.t option }

    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 Job} *)

  module Job : sig
    type t = {
      name : string option;
          (** Output only, e.g.
              ["projects/123456/locations/us-central1/jobs/job01"]. *)
      uid : string option;  (** Output only. *)
      priority : int option;  (** [0, 100). Higher runs first. *)
      task_groups : Task_group.t list;
          (** Required. Only one task group is currently supported. *)
      allocation_policy : Allocation_policy.t option;
      labels : (string * string) list;
      status : Job_status.t option;  (** Output only. *)
      create_time : string option;  (** Output only. *)
      update_time : string option;  (** Output only. *)
      logs_policy : Logs_policy.t option;
      notifications : Job_notification.t list;
    }

    val make :
      task_groups:Task_group.t list ->
      ?priority:int ->
      ?allocation_policy:Allocation_policy.t ->
      ?labels:(string * string) list ->
      ?logs_policy:Logs_policy.t ->
      ?notifications:Job_notification.t list ->
      unit ->
      t

    val id : t -> string option
    (** The last segment of [name]: the job ID usable as [~job] below. *)

    val state : t -> Job_state.t
    (** [STATE_UNSPECIFIED] when [status] is absent. *)

    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_jobs_response : sig
    type t = {
      jobs : Job.t list;
      next_page_token : string option;
      unreachable : string list;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_tasks_response : sig
    type t = {
      tasks : Task.t list;
      next_page_token : string option;
      unreachable : string list;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 API methods}

      [location] is a region such as ["us-central1"]. [project_id] falls back
      to the usual discovery (environment, credentials, Cloud SDK config). *)

  module Projects : sig
    module Locations : sig
      module Jobs : sig
        val create :
          ?project_id:string ->
          location:string ->
          ?job_id:string ->
          ?request_id:string ->
          Job.t ->
          (Job.t, [> Error.t ]) Lwt_result.t
        (** Create a job. [job_id] must match [[a-z]([a-z0-9-]{0,61}[a-z0-9])?];
            a random one is generated if omitted. *)

        val get :
          ?project_id:string ->
          location:string ->
          job:string ->
          unit ->
          (Job.t, [> Error.t ]) Lwt_result.t

        val list :
          ?project_id:string ->
          location:string ->
          ?filter:string ->
          ?order_by:string ->
          ?page_size:int ->
          ?page_token:string ->
          unit ->
          (List_jobs_response.t, [> Error.t ]) Lwt_result.t
        (** [order_by] is one of ["name"], ["name desc"], ["create_time"],
            ["create_time desc"]. *)

        val delete :
          ?project_id:string ->
          location:string ->
          ?reason:string ->
          ?request_id:string ->
          job:string ->
          unit ->
          (Operation.t, [> Error.t ]) Lwt_result.t
        (** Delete a job. Returns a long-running operation; see
            {!Operations.get}. *)

        val cancel :
          ?project_id:string ->
          location:string ->
          ?request_id:string ->
          job:string ->
          unit ->
          (Operation.t, [> Error.t ]) Lwt_result.t

        val poll_until_complete :
          ?project_id:string ->
          location:string ->
          ?poll_every_s:float ->
          ?timeout_s:float ->
          job:string ->
          unit ->
          (Job.t, [> Error.t ]) Lwt_result.t
        (** Poll {!get} every [poll_every_s] seconds (default 10) until the job
            reaches a terminal state ({!Job_state.is_terminal}) and return it.
            Fails with [`Gcloud_retry_timeout] once [timeout_s] elapses; waits
            indefinitely when [timeout_s] is omitted. Whether the job succeeded
            is left to the caller: check {!Job.state}. *)

        module TaskGroups : sig
          module Tasks : sig
            val get :
              ?project_id:string ->
              location:string ->
              job:string ->
              ?task_group:string ->
              task:string ->
              unit ->
              (Task.t, [> Error.t ]) Lwt_result.t
            (** [task_group] defaults to ["group0"]; [task] is the task index,
                e.g. ["0"]. *)

            val list :
              ?project_id:string ->
              location:string ->
              job:string ->
              ?task_group:string ->
              ?filter:string ->
              ?page_size:int ->
              ?page_token:string ->
              unit ->
              (List_tasks_response.t, [> Error.t ]) Lwt_result.t
            (** [filter] is of the form ["State=RUNNING"]. *)
          end
        end
      end

      module Operations : sig
        val get :
          name:string -> unit -> (Operation.t, [> Error.t ]) Lwt_result.t
        (** [name] is the full operation name as returned in {!Operation.name}. *)

        val list :
          ?project_id:string ->
          location:string ->
          ?filter:string ->
          ?page_size:int ->
          ?page_token:string ->
          unit ->
          (List_operations_response.t, [> Error.t ]) Lwt_result.t

        val cancel : name:string -> unit -> (unit, [> Error.t ]) Lwt_result.t
        val delete : name:string -> unit -> (unit, [> Error.t ]) Lwt_result.t
      end
    end
  end
end

(** {1 v1alpha} *)

module V1alpha : sig
  include module type of struct
    include Batch_types
  end

  (** {2 Alpha-only enums} *)

  module Job_dependency_type : sig
    type t = TYPE_UNSPECIFIED | SUCCEEDED | FAILED | FINISHED

    include ENUM with type t := t
  end

  module Calendar_period : sig
    type t = CALENDAR_PERIOD_UNSPECIFIED | MONTH | QUARTER | YEAR | WEEK | DAY

    include ENUM with type t := t
  end

  module Resource_allowance_state : sig
    type t =
      | RESOURCE_ALLOWANCE_STATE_UNSPECIFIED
      | RESOURCE_ALLOWANCE_ACTIVE
      | RESOURCE_ALLOWANCE_DEPLETED

    include ENUM with type t := t
  end

  (** {2 Job specification} *)

  module Job_dependency : sig
    type t = { items : (string * Job_dependency_type.t) list }
    (** Maps a job name to the state it must reach. All items must be
        satisfied. *)

    val make : items:(string * Job_dependency_type.t) list -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Instance_selection : sig
    type t = {
      rank : int option;  (** Lower is preferred. *)
      boot_disk : Disk.t option;
      disks : Attached_disk.t list;
      machine_types : string list;  (** e.g. ["n1-standard-16"] *)
    }

    val make :
      machine_types:string list ->
      ?rank:int ->
      ?boot_disk:Disk.t ->
      ?disks:Attached_disk.t list ->
      unit ->
      t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Instance_flexibility_policy : sig
    type t = { instance_selections : (string * Instance_selection.t) list }
    (** Keyed by a user-chosen selection name. *)

    val make : instance_selections:(string * Instance_selection.t) list -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Allocation_policy : sig
    type t = {
      location : Location_policy.t option;
      instances : Instance_policy_or_template.t list;
      instance_flexibility_policy : Instance_flexibility_policy.t option;
      service_account : Service_account.t option;
      labels : (string * string) list;
      network : Network_policy.t option;
      placement : Placement_policy.t option;
      tags : string list;
    }

    val make :
      ?location:Location_policy.t ->
      ?instances:Instance_policy_or_template.t list ->
      ?instance_flexibility_policy:Instance_flexibility_policy.t ->
      ?service_account:Service_account.t ->
      ?labels:(string * string) list ->
      ?network:Network_policy.t ->
      ?placement:Placement_policy.t ->
      ?tags:string list ->
      unit ->
      t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task_group : sig
    type t = {
      name : string option;  (** Output only. *)
      task_spec : Task_spec.t;
      task_count : int option;
      parallelism : int option;
      scheduling_policy : Scheduling_policy.t option;
      allocation_policy : Allocation_policy.t option;
          (** Overrides the job-level allocation policy. *)
      labels : (string * string) list;
      task_environments : Environment.t list;
      task_count_per_node : int option;
      require_hosts_file : bool;
      permissive_ssh : bool;
      run_as_non_root : bool;
      service_account : Service_account.t option;
    }

    val make :
      task_spec:Task_spec.t ->
      ?task_count:int ->
      ?parallelism:int ->
      ?scheduling_policy:Scheduling_policy.t ->
      ?allocation_policy:Allocation_policy.t ->
      ?labels:(string * string) list ->
      ?task_environments:Environment.t list ->
      ?task_count_per_node:int ->
      ?require_hosts_file:bool ->
      ?permissive_ssh:bool ->
      ?run_as_non_root:bool ->
      ?service_account:Service_account.t ->
      unit ->
      t

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 Status (output only)} *)

  module Resource_usage : sig
    type t = { core_hours : float option }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task_execution : sig
    type t = { exit_code : int option; stderr_snippet : string option }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Status_event : sig
    type t = {
      type_ : string option;
      description : string option;
      event_time : string option;
      task_execution : Task_execution.t option;
      task_state : Task_state.t option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Job_status : sig
    type t = {
      state : Job_state.t;
      status_events : Status_event.t list;
      task_groups : (string * Task_group_status.t) list;
          (** Keyed by task group ID, e.g. ["group0"]. *)
      run_duration : string option;
      resource_usage : Resource_usage.t option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task_status : sig
    type t = {
      state : Task_state.t;
      status_events : Status_event.t list;
      resource_usage : Resource_usage.t option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Task : sig
    type t = { name : string; status : Task_status.t option }

    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 Job} *)

  module Job : sig
    type t = {
      name : string option;  (** Output only. *)
      uid : string option;  (** Output only. *)
      priority : int option;  (** [0, 100). Higher runs first. *)
      task_groups : Task_group.t list;  (** Required. *)
      scheduling_policy : Scheduling_policy.t option;
      dependencies : Job_dependency.t list;
          (** At least one dependency must be satisfied before the job is
              scheduled. *)
      allocation_policy : Allocation_policy.t option;
      labels : (string * string) list;
      status : Job_status.t option;  (** Output only. *)
      create_time : string option;  (** Output only. *)
      update_time : string option;  (** Output only. *)
      logs_policy : Logs_policy.t option;
      notifications : Job_notification.t list;
    }

    val make :
      task_groups:Task_group.t list ->
      ?priority:int ->
      ?scheduling_policy:Scheduling_policy.t ->
      ?dependencies:Job_dependency.t list ->
      ?allocation_policy:Allocation_policy.t ->
      ?labels:(string * string) list ->
      ?logs_policy:Logs_policy.t ->
      ?notifications:Job_notification.t list ->
      unit ->
      t

    val id : t -> string option
    val state : t -> Job_state.t
    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_jobs_response : sig
    type t = {
      jobs : Job.t list;
      next_page_token : string option;
      unreachable : string list;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_tasks_response : sig
    type t = {
      tasks : Task.t list;
      next_page_token : string option;
      unreachable : string list;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 Resource allowances}

      A resource allowance caps the usage (currently CPU core hours) that jobs
      in a project and location may consume per calendar period. *)

  module Interval : sig
    type t = { start_time : string option; end_time : string option }

    val make : ?start_time:string -> ?end_time:string -> unit -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Limit : sig
    type t = {
      calendar_period : Calendar_period.t option;
      limit : float option;
    }

    val make : ?calendar_period:Calendar_period.t -> ?limit:float -> unit -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Limit_status : sig
    type t = {
      consumption_interval : Interval.t option;
      limit : float option;
      consumed : float option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Period_consumption : sig
    type t = {
      consumption_interval : Interval.t option;
      consumed : float option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Consumption_report : sig
    type t = {
      latest_period_consumptions : (string * Period_consumption.t) list;
    }
    (** Keyed by calendar period name, e.g. ["MONTH"]. *)

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Usage_resource_allowance_status : sig
    type t = {
      state : Resource_allowance_state.t;
      report : Consumption_report.t option;
      limit_status : Limit_status.t option;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Usage_resource_allowance_spec : sig
    type t = {
      type_ : string;  (** Currently only ["cpu-core-hours"]. *)
      limit : Limit.t;
    }

    val make : type_:string -> limit:Limit.t -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Usage_resource_allowance : sig
    type t = {
      spec : Usage_resource_allowance_spec.t;
      status : Usage_resource_allowance_status.t option;  (** Output only. *)
    }

    val make : spec:Usage_resource_allowance_spec.t -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Notification : sig
    type t = { pubsub_topic : string }

    val make : pubsub_topic:string -> t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module Resource_allowance : sig
    type t = {
      name : string option;
          (** e.g.
              ["projects/123456/locations/us-central1/resourceAllowances/ra-1"] *)
      uid : string option;  (** Output only. *)
      usage_resource_allowance : Usage_resource_allowance.t option;
      notifications : Notification.t list;
      labels : (string * string) list;
      create_time : string option;  (** Output only. *)
    }

    val make :
      ?name:string ->
      ?usage_resource_allowance:Usage_resource_allowance.t ->
      ?notifications:Notification.t list ->
      ?labels:(string * string) list ->
      unit ->
      t

    val id : t -> string option
    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module List_resource_allowances_response : sig
    type t = {
      resource_allowances : Resource_allowance.t list;
      next_page_token : string option;
      unreachable : string list;
    }

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  (** {2 API methods}

      Same conventions as {!V1.Projects}. *)

  module Projects : sig
    module Locations : sig
      module Jobs : sig
        val create :
          ?project_id:string ->
          location:string ->
          ?job_id:string ->
          ?request_id:string ->
          Job.t ->
          (Job.t, [> Error.t ]) Lwt_result.t

        val get :
          ?project_id:string ->
          location:string ->
          job:string ->
          unit ->
          (Job.t, [> Error.t ]) Lwt_result.t

        val list :
          ?project_id:string ->
          location:string ->
          ?filter:string ->
          ?order_by:string ->
          ?page_size:int ->
          ?page_token:string ->
          unit ->
          (List_jobs_response.t, [> Error.t ]) Lwt_result.t

        val delete :
          ?project_id:string ->
          location:string ->
          ?reason:string ->
          ?request_id:string ->
          job:string ->
          unit ->
          (Operation.t, [> Error.t ]) Lwt_result.t

        val cancel :
          ?project_id:string ->
          location:string ->
          ?request_id:string ->
          job:string ->
          unit ->
          (Operation.t, [> Error.t ]) Lwt_result.t

        val patch :
          ?project_id:string ->
          location:string ->
          ?request_id:string ->
          update_mask:string ->
          job:string ->
          Job.t ->
          (Job.t, [> Error.t ]) Lwt_result.t
        (** Update a queued, scheduled or running job. Currently only
            increasing the first task group's [task_count] is supported, so
            [update_mask] must be ["taskGroups[0].taskCount"] (or
            ["task_groups[0].task_count"]). *)

        val poll_until_complete :
          ?project_id:string ->
          location:string ->
          ?poll_every_s:float ->
          ?timeout_s:float ->
          job:string ->
          unit ->
          (Job.t, [> Error.t ]) Lwt_result.t

        module TaskGroups : sig
          module Tasks : sig
            val get :
              ?project_id:string ->
              location:string ->
              job:string ->
              ?task_group:string ->
              task:string ->
              unit ->
              (Task.t, [> Error.t ]) Lwt_result.t

            val list :
              ?project_id:string ->
              location:string ->
              job:string ->
              ?task_group:string ->
              ?filter:string ->
              ?order_by:string ->
              ?page_size:int ->
              ?page_token:string ->
              unit ->
              (List_tasks_response.t, [> Error.t ]) Lwt_result.t
          end
        end
      end

      module Operations : sig
        val get :
          name:string -> unit -> (Operation.t, [> Error.t ]) Lwt_result.t

        val list :
          ?project_id:string ->
          location:string ->
          ?filter:string ->
          ?page_size:int ->
          ?page_token:string ->
          unit ->
          (List_operations_response.t, [> Error.t ]) Lwt_result.t

        val cancel : name:string -> unit -> (unit, [> Error.t ]) Lwt_result.t
        val delete : name:string -> unit -> (unit, [> Error.t ]) Lwt_result.t
      end

      module ResourceAllowances : sig
        val create :
          ?project_id:string ->
          location:string ->
          ?resource_allowance_id:string ->
          ?request_id:string ->
          Resource_allowance.t ->
          (Resource_allowance.t, [> Error.t ]) Lwt_result.t

        val get :
          ?project_id:string ->
          location:string ->
          resource_allowance:string ->
          unit ->
          (Resource_allowance.t, [> Error.t ]) Lwt_result.t

        val list :
          ?project_id:string ->
          location:string ->
          ?page_size:int ->
          ?page_token:string ->
          unit ->
          (List_resource_allowances_response.t, [> Error.t ]) Lwt_result.t

        val delete :
          ?project_id:string ->
          location:string ->
          ?reason:string ->
          ?request_id:string ->
          resource_allowance:string ->
          unit ->
          (Operation.t, [> Error.t ]) Lwt_result.t

        val patch :
          ?project_id:string ->
          location:string ->
          ?request_id:string ->
          update_mask:string ->
          resource_allowance:string ->
          Resource_allowance.t ->
          (Resource_allowance.t, [> Error.t ]) Lwt_result.t
      end
    end
  end
end
