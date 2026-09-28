(** Types shared by every version of the Cloud Batch API.

    {!Batch.V1} and {!Batch.V1alpha} re-export everything here, so you
    normally use e.g. [Batch.V1.Script] rather than this module directly. *)

(** {1 JSON helpers}

    Used by the derived converters; exposed so that version-specific modules
    can reuse them. *)

(** Google's JSON mapping encodes [int64] fields as decimal strings. *)
module Int64_string : sig
  type t = int

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** JSON objects used as maps. *)
module Assoc : sig
  val to_yojson : ('a -> Yojson.Safe.t) -> (string * 'a) list -> Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ((string * 'a) list, string) result
end

module String_map : sig
  val to_yojson : (string * string) list -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> ((string * string) list, string) result
end

(** {1 Enums}

    Enums are serialised as their (upper-case) constructor name. Parsing a
    value not listed here yields a [`Json_transform_error]. *)

module Enum : sig
  module type S = sig
    type t

    val min : int
    val max : int
    val to_enum : t -> int
    val of_enum : int -> t option
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Make (E : S) : sig
    val all : E.t list
    val to_string : E.t -> string
    val of_string : string -> E.t option
    val to_yojson : E.t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (E.t, string) result
  end
end

module type ENUM = sig
  type t

  val all : t list
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Job_state : sig
  type t =
    | STATE_UNSPECIFIED
    | QUEUED
    | SCHEDULED
    | RUNNING
    | SUCCEEDED
    | FAILED
    | DELETION_IN_PROGRESS
    | CANCELLATION_IN_PROGRESS
    | CANCELLED

  include ENUM with type t := t

  val is_terminal : t -> bool
  (** [true] for [SUCCEEDED], [FAILED] and [CANCELLED]. *)
end

module Task_state : sig
  type t =
    | STATE_UNSPECIFIED
    | PENDING
    | ASSIGNED
    | RUNNING
    | FAILED
    | SUCCEEDED
    | UNEXECUTED

  include ENUM with type t := t
end

module Scheduling_policy : sig
  type t = SCHEDULING_POLICY_UNSPECIFIED | AS_SOON_AS_POSSIBLE | IN_ORDER

  include ENUM with type t := t
end

module Provisioning_model : sig
  type t =
    | PROVISIONING_MODEL_UNSPECIFIED
    | STANDARD
    | SPOT
    | PREEMPTIBLE
    | RESERVATION_BOUND
    | FLEX_START

  include ENUM with type t := t
end

module Lifecycle_action : sig
  type t = ACTION_UNSPECIFIED | RETRY_TASK | FAIL_TASK

  include ENUM with type t := t
end

module Logs_destination : sig
  type t = DESTINATION_UNSPECIFIED | CLOUD_LOGGING | PATH

  include ENUM with type t := t
end

module Nic_type : sig
  type t = NIC_TYPE_UNSPECIFIED | GVNIC | IRDMA | MRDMA

  include ENUM with type t := t
end

module Message_type : sig
  type t = TYPE_UNSPECIFIED | JOB_STATE_CHANGED | TASK_STATE_CHANGED

  include ENUM with type t := t
end

(** {1 Job specification}

    Records mirror the REST resources. [option] fields are omitted from the
    request when [None]; [bool] fields default to [false]; [int64] API fields
    (serialised as decimal strings by Google) are plain [int]s here. Each
    record has a [make] constructor with optional arguments for everything
    that is not required. Durations are strings such as ["3600s"]. *)

module Script : sig
  type t = { path : string option; text : string option }

  val make : ?path:string -> ?text:string -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Container : sig
  type t = {
    image_uri : string;
    commands : string list;
    entrypoint : string option;
    volumes : string list;
    options : string option;
    block_external_network : bool;
    username : string option;
    password : string option;
    enable_image_streaming : bool;
  }

  val make :
    image_uri:string ->
    ?commands:string list ->
    ?entrypoint:string ->
    ?volumes:string list ->
    ?options:string ->
    ?block_external_network:bool ->
    ?username:string ->
    ?password:string ->
    ?enable_image_streaming:bool ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Barrier : sig
  type t = { name : string option }

  val make : ?name:string -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Kms_env_map : sig
  type t = { key_name : string; cipher_text : string }

  val make : key_name:string -> cipher_text:string -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Environment : sig
  type t = {
    variables : (string * string) list;
    secret_variables : (string * string) list;
        (** Environment variable name to Secret Manager secret name. *)
    encrypted_variables : Kms_env_map.t option;
  }

  val make :
    ?variables:(string * string) list ->
    ?secret_variables:(string * string) list ->
    ?encrypted_variables:Kms_env_map.t ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Gcs : sig
  type t = { remote_path : string }

  val make : remote_path:string -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Nfs : sig
  type t = { server : string; remote_path : string }

  val make : server:string -> remote_path:string -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Volume : sig
  type t = {
    gcs : Gcs.t option;
    nfs : Nfs.t option;
    device_name : string option;
    mount_path : string;
    mount_options : string list;
  }

  val make :
    ?gcs:Gcs.t ->
    ?nfs:Nfs.t ->
    ?device_name:string ->
    mount_path:string ->
    ?mount_options:string list ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Compute_resource : sig
  type t = {
    cpu_milli : int option;
    memory_mib : int option;
    boot_disk_mib : int option;
  }

  val make :
    ?cpu_milli:int -> ?memory_mib:int -> ?boot_disk_mib:int -> unit -> t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Action_condition : sig
  type t = { exit_codes : int list }

  val make : ?exit_codes:int list -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Lifecycle_policy : sig
  type t = {
    action : Lifecycle_action.t option;
    action_condition : Action_condition.t option;
  }

  val make :
    ?action:Lifecycle_action.t ->
    ?action_condition:Action_condition.t ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Runnable : sig
  type t = {
    script : Script.t option;
    container : Container.t option;
    barrier : Barrier.t option;
    display_name : string option;
    ignore_exit_status : bool;
    background : bool;
    always_run : bool;
    environment : Environment.t option;
    timeout : string option;
    labels : (string * string) list;
  }

  val make :
    ?script:Script.t ->
    ?container:Container.t ->
    ?barrier:Barrier.t ->
    ?display_name:string ->
    ?ignore_exit_status:bool ->
    ?background:bool ->
    ?always_run:bool ->
    ?environment:Environment.t ->
    ?timeout:string ->
    ?labels:(string * string) list ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Task_spec : sig
  type t = {
    runnables : Runnable.t list;
    compute_resource : Compute_resource.t option;
    max_run_duration : string option;
    max_retry_count : int option;
    lifecycle_policies : Lifecycle_policy.t list;
    environment : Environment.t option;
    volumes : Volume.t list;
  }

  val make :
    runnables:Runnable.t list ->
    ?compute_resource:Compute_resource.t ->
    ?max_run_duration:string ->
    ?max_retry_count:int ->
    ?lifecycle_policies:Lifecycle_policy.t list ->
    ?environment:Environment.t ->
    ?volumes:Volume.t list ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** {2 Allocation policy} *)

module Disk : sig
  type t = {
    type_ : string option;  (** e.g. ["pd-balanced"], ["local-ssd"] *)
    size_gb : int option;
    disk_interface : string option;
    image : string option;
    snapshot : string option;
  }

  val make :
    ?type_:string ->
    ?size_gb:int ->
    ?disk_interface:string ->
    ?image:string ->
    ?snapshot:string ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Attached_disk : sig
  type t = {
    new_disk : Disk.t option;
    existing_disk : string option;
    device_name : string option;
  }

  val make :
    ?new_disk:Disk.t ->
    ?existing_disk:string ->
    ?device_name:string ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Accelerator : sig
  type t = {
    type_ : string option;  (** e.g. ["nvidia-tesla-t4"] *)
    count : int option;
    driver_version : string option;
  }

  val make : ?type_:string -> ?count:int -> ?driver_version:string -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Instance_policy : sig
  type t = {
    machine_type : string option;
    min_cpu_platform : string option;
    provisioning_model : Provisioning_model.t option;
    accelerators : Accelerator.t list;
    boot_disk : Disk.t option;
    disks : Attached_disk.t list;
    reservation : string option;
  }

  val make :
    ?machine_type:string ->
    ?min_cpu_platform:string ->
    ?provisioning_model:Provisioning_model.t ->
    ?accelerators:Accelerator.t list ->
    ?boot_disk:Disk.t ->
    ?disks:Attached_disk.t list ->
    ?reservation:string ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Instance_policy_or_template : sig
  type t = {
    policy : Instance_policy.t option;
    instance_template : string option;
    install_gpu_drivers : bool;
    install_ops_agent : bool;
    block_project_ssh_keys : bool;
  }

  val make :
    ?policy:Instance_policy.t ->
    ?instance_template:string ->
    ?install_gpu_drivers:bool ->
    ?install_ops_agent:bool ->
    ?block_project_ssh_keys:bool ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Location_policy : sig
  type t = { allowed_locations : string list }

  val make : ?allowed_locations:string list -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Network_interface : sig
  type t = {
    network : string option;
    subnetwork : string option;
    no_external_ip_address : bool;
    nic_type : Nic_type.t option;
  }

  val make :
    ?network:string ->
    ?subnetwork:string ->
    ?no_external_ip_address:bool ->
    ?nic_type:Nic_type.t ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Network_policy : sig
  type t = { network_interfaces : Network_interface.t list }

  val make : ?network_interfaces:Network_interface.t list -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Placement_policy : sig
  type t = { collocation : string option; max_distance : int option }

  val make : ?collocation:string -> ?max_distance:int -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Service_account : sig
  type t = { email : string option; scopes : string list }

  val make : ?email:string -> ?scopes:string list -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** {2 Logs and notifications} *)

module Cloud_logging_option : sig
  type t = { use_generic_task_monitored_resource : bool }

  val make : ?use_generic_task_monitored_resource:bool -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Logs_policy : sig
  type t = {
    destination : Logs_destination.t option;
    logs_path : string option;
    cloud_logging_option : Cloud_logging_option.t option;
  }

  val make :
    ?destination:Logs_destination.t ->
    ?logs_path:string ->
    ?cloud_logging_option:Cloud_logging_option.t ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Message : sig
  type t = {
    type_ : Message_type.t option;
    new_job_state : Job_state.t option;
    new_task_state : Task_state.t option;
  }

  val make :
    ?type_:Message_type.t ->
    ?new_job_state:Job_state.t ->
    ?new_task_state:Task_state.t ->
    unit ->
    t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Job_notification : sig
  type t = { pubsub_topic : string option; message : Message.t option }

  val make : ?pubsub_topic:string -> ?message:Message.t -> unit -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** {1 Status (output only)} *)

module Instance_status : sig
  type t = {
    machine_type : string option;
    provisioning_model : Provisioning_model.t option;
    task_pack : int option;
    boot_disk : Disk.t option;
  }

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Task_group_status : sig
  type t = {
    counts : (string * int) list;
        (** Number of tasks in each state, keyed by task state name. *)
    instances : Instance_status.t list;
  }

  val to_yojson : t -> Yojson.Safe.t
  val map_to_yojson : (string * t) list -> Yojson.Safe.t
  val map_of_yojson : Yojson.Safe.t -> ((string * t) list, string) result
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** {1 Long-running operations} *)

module Rpc_status : sig
  type t = {
    code : int option;  (** A [google.rpc.Code] value. *)
    message : string option;
    details : Yojson.Safe.t list;
  }

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Operation_metadata : sig
  type t = {
    create_time : string option;
    end_time : string option;
    target : string option;
    verb : string option;
    status_message : string option;
    requested_cancellation : bool;
    api_version : string option;
  }

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Operation : sig
  type t = {
    name : string;
        (** e.g. ["projects/123/locations/us-central1/operations/<uuid>"] *)
    done_ : bool;
    metadata : Yojson.Safe.t option;
    error : Rpc_status.t option;
    response : Yojson.Safe.t option;
  }

  val metadata : t -> (Operation_metadata.t option, string) result
  (** Decode [metadata] as the Batch [OperationMetadata] message. *)

  val pp : Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module List_operations_response : sig
  type t = {
    operations : Operation.t list;
    next_page_token : string option;
    unreachable : string list;
  }

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
