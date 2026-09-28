(** Types shared by every version of the Cloud Batch API.

    See {!Batch.V1} and {!Batch.V1alpha}, which re-export everything here
    alongside the version-specific resources and API methods. *)

(* {1 JSON helpers} *)

module Int64_string = struct
  type t = int

  let to_yojson (i : t) : Yojson.Safe.t = `String (string_of_int i)

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `String s | `Intlit s -> (
        match int_of_string_opt s with
        | Some i -> Ok i
        | None -> Error (Printf.sprintf "Int64_string: %S is not an integer" s))
    | `Int i -> Ok i
    | j ->
        Error
          (Printf.sprintf "Int64_string: expected a string, got %s"
             (Yojson.Safe.to_string j))
end

module Assoc = struct
  let to_yojson (value_to_yojson : 'a -> Yojson.Safe.t) (m : (string * 'a) list)
      : Yojson.Safe.t =
    `Assoc (List.map (fun (k, v) -> (k, value_to_yojson v)) m)

  let of_yojson (value_of_yojson : Yojson.Safe.t -> ('a, string) result) :
      Yojson.Safe.t -> ((string * 'a) list, string) result = function
    | `Assoc kvs ->
        kvs
        |> List.map (fun (k, j) ->
               value_of_yojson j
               |> CCResult.map (fun v -> (k, v))
               |> CCResult.map_err (fun e -> Printf.sprintf "key %S: %s" k e))
        |> CCList.all_ok
    | j ->
        Error
          (Printf.sprintf "expected an object, got %s" (Yojson.Safe.to_string j))
end

module String_map = struct
  let to_yojson = Assoc.to_yojson (fun s -> `String s)

  let of_yojson =
    Assoc.of_yojson (function
      | `String s -> Ok s
      | j ->
          Error
            (Printf.sprintf "expected a string, got %s"
               (Yojson.Safe.to_string j)))
end

module Enum = struct
  module type S = sig
    type t

    val min : int
    val max : int
    val to_enum : t -> int
    val of_enum : int -> t option
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Make (E : S) = struct
    let all : E.t list =
      List.init
        (E.max - E.min + 1)
        (fun i ->
          match E.of_enum (i + E.min) with Some v -> v | None -> assert false)

    let to_string = E.show

    let of_string (s : string) : E.t option =
      List.find_opt (fun v -> String.equal (E.show v) s) all

    let to_yojson (v : E.t) : Yojson.Safe.t = `String (E.show v)

    let of_yojson : Yojson.Safe.t -> (E.t, string) result = function
      | `String s -> (
          match of_string s with
          | Some v -> Ok v
          | None ->
              Error
                (Printf.sprintf "expected one of [%s], got %S"
                   (String.concat "; " (List.map E.show all))
                   s))
      | j ->
          Error
            (Printf.sprintf "expected a string, got %s"
               (Yojson.Safe.to_string j))
  end
end

(* {1 Enums} *)

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

module Job_state = struct
  module T = struct
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
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)

  let is_terminal = function
    | SUCCEEDED | FAILED | CANCELLED -> true
    | STATE_UNSPECIFIED | QUEUED | SCHEDULED | RUNNING | DELETION_IN_PROGRESS
    | CANCELLATION_IN_PROGRESS ->
        false
end

module Task_state = struct
  module T = struct
    type t =
      | STATE_UNSPECIFIED
      | PENDING
      | ASSIGNED
      | RUNNING
      | FAILED
      | SUCCEEDED
      | UNEXECUTED
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Scheduling_policy = struct
  module T = struct
    type t = SCHEDULING_POLICY_UNSPECIFIED | AS_SOON_AS_POSSIBLE | IN_ORDER
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Provisioning_model = struct
  module T = struct
    type t =
      | PROVISIONING_MODEL_UNSPECIFIED
      | STANDARD
      | SPOT
      | PREEMPTIBLE
      | RESERVATION_BOUND
      | FLEX_START
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Lifecycle_action = struct
  module T = struct
    type t = ACTION_UNSPECIFIED | RETRY_TASK | FAIL_TASK
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Logs_destination = struct
  module T = struct
    type t = DESTINATION_UNSPECIFIED | CLOUD_LOGGING | PATH
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Nic_type = struct
  module T = struct
    type t = NIC_TYPE_UNSPECIFIED | GVNIC | IRDMA | MRDMA
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

module Message_type = struct
  module T = struct
    type t = TYPE_UNSPECIFIED | JOB_STATE_CHANGED | TASK_STATE_CHANGED
    [@@deriving show { with_path = false }, enum]
  end

  include T
  include Enum.Make (T)
end

(* {1 Resources}

   Field conventions:
   - [_ option] fields with [@yojson.default None] are omitted from the
     request body when [None], and tolerate being absent in responses.
     The [yojson.]-qualified attribute keeps [@@deriving make] generating
     plain [?field:'a] arguments rather than [?field:'a option].
   - [bool] fields default to [false], matching proto3 semantics.
   - [int64] fields are typed as [Int64_string.t] (= [int]). *)

[@@@warning "-39"]

module Script = struct
  type t = {
    path : string option; [@yojson.default None]
    text : string option; [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Container = struct
  type t = {
    image_uri : string; [@key "imageUri"]
    commands : string list; [@default []]
    entrypoint : string option; [@yojson.default None]
    volumes : string list; [@default []]
    options : string option; [@yojson.default None]
    block_external_network : bool;
        [@key "blockExternalNetwork"] [@default false]
    username : string option; [@yojson.default None]
    password : string option; [@yojson.default None]
    enable_image_streaming : bool; [@key "enableImageStreaming"] [@default false]
  }
  [@@deriving yojson { strict = false }, make]
end

module Barrier = struct
  type t = { name : string option [@yojson.default None] }
  [@@deriving yojson { strict = false }, make]
end

module Kms_env_map = struct
  type t = {
    key_name : string; [@key "keyName"]
    cipher_text : string; [@key "cipherText"]
  }
  [@@deriving yojson { strict = false }, make]
end

module Environment = struct
  type t = {
    variables : (string * string) list;
        [@default []]
        [@to_yojson String_map.to_yojson]
        [@of_yojson String_map.of_yojson]
    secret_variables : (string * string) list;
        [@key "secretVariables"]
        [@default []]
        [@to_yojson String_map.to_yojson]
        [@of_yojson String_map.of_yojson]
    encrypted_variables : Kms_env_map.t option;
        [@key "encryptedVariables"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Gcs = struct
  type t = { remote_path : string [@key "remotePath"] }
  [@@deriving yojson { strict = false }, make]
end

module Nfs = struct
  type t = { server : string; remote_path : string [@key "remotePath"] }
  [@@deriving yojson { strict = false }, make]
end

module Volume = struct
  type t = {
    gcs : Gcs.t option; [@yojson.default None]
    nfs : Nfs.t option; [@yojson.default None]
    device_name : string option; [@key "deviceName"] [@yojson.default None]
    mount_path : string; [@key "mountPath"]
    mount_options : string list; [@key "mountOptions"] [@default []]
  }
  [@@deriving yojson { strict = false }, make]
end

module Compute_resource = struct
  type t = {
    cpu_milli : Int64_string.t option; [@key "cpuMilli"] [@yojson.default None]
    memory_mib : Int64_string.t option;
        [@key "memoryMib"] [@yojson.default None]
    boot_disk_mib : Int64_string.t option;
        [@key "bootDiskMib"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Action_condition = struct
  type t = { exit_codes : int list [@key "exitCodes"] [@default []] }
  [@@deriving yojson { strict = false }, make]
end

module Lifecycle_policy = struct
  type t = {
    action : Lifecycle_action.t option; [@yojson.default None]
    action_condition : Action_condition.t option;
        [@key "actionCondition"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Runnable = struct
  type t = {
    script : Script.t option; [@yojson.default None]
    container : Container.t option; [@yojson.default None]
    barrier : Barrier.t option; [@yojson.default None]
    display_name : string option; [@key "displayName"] [@yojson.default None]
    ignore_exit_status : bool; [@key "ignoreExitStatus"] [@default false]
    background : bool; [@default false]
    always_run : bool; [@key "alwaysRun"] [@default false]
    environment : Environment.t option; [@yojson.default None]
    timeout : string option; [@yojson.default None]
    labels : (string * string) list;
        [@default []]
        [@to_yojson String_map.to_yojson]
        [@of_yojson String_map.of_yojson]
  }
  [@@deriving yojson { strict = false }, make]
end

module Task_spec = struct
  type t = {
    runnables : Runnable.t list; [@default []]
    compute_resource : Compute_resource.t option;
        [@key "computeResource"] [@yojson.default None]
    max_run_duration : string option;
        [@key "maxRunDuration"] [@yojson.default None]
    max_retry_count : int option; [@key "maxRetryCount"] [@yojson.default None]
    lifecycle_policies : Lifecycle_policy.t list;
        [@key "lifecyclePolicies"] [@default []]
    environment : Environment.t option; [@yojson.default None]
    volumes : Volume.t list; [@default []]
  }
  [@@deriving yojson { strict = false }]

  let make ~runnables ?compute_resource ?max_run_duration ?max_retry_count
      ?(lifecycle_policies = []) ?environment ?(volumes = []) () =
    {
      runnables;
      compute_resource;
      max_run_duration;
      max_retry_count;
      lifecycle_policies;
      environment;
      volumes;
    }
end

module Disk = struct
  type t = {
    type_ : string option; [@key "type"] [@yojson.default None]
    size_gb : Int64_string.t option; [@key "sizeGb"] [@yojson.default None]
    disk_interface : string option;
        [@key "diskInterface"] [@yojson.default None]
    image : string option; [@yojson.default None]
    snapshot : string option; [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Attached_disk = struct
  type t = {
    new_disk : Disk.t option; [@key "newDisk"] [@yojson.default None]
    existing_disk : string option; [@key "existingDisk"] [@yojson.default None]
    device_name : string option; [@key "deviceName"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Accelerator = struct
  type t = {
    type_ : string option; [@key "type"] [@yojson.default None]
    count : Int64_string.t option; [@yojson.default None]
    driver_version : string option; [@key "driverVersion"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Instance_policy = struct
  type t = {
    machine_type : string option; [@key "machineType"] [@yojson.default None]
    min_cpu_platform : string option;
        [@key "minCpuPlatform"] [@yojson.default None]
    provisioning_model : Provisioning_model.t option;
        [@key "provisioningModel"] [@yojson.default None]
    accelerators : Accelerator.t list; [@default []]
    boot_disk : Disk.t option; [@key "bootDisk"] [@yojson.default None]
    disks : Attached_disk.t list; [@default []]
    reservation : string option; [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Instance_policy_or_template = struct
  type t = {
    policy : Instance_policy.t option; [@yojson.default None]
    instance_template : string option;
        [@key "instanceTemplate"] [@yojson.default None]
    install_gpu_drivers : bool; [@key "installGpuDrivers"] [@default false]
    install_ops_agent : bool; [@key "installOpsAgent"] [@default false]
    block_project_ssh_keys : bool; [@key "blockProjectSshKeys"] [@default false]
  }
  [@@deriving yojson { strict = false }, make]
end

module Location_policy = struct
  type t = {
    allowed_locations : string list; [@key "allowedLocations"] [@default []]
  }
  [@@deriving yojson { strict = false }, make]
end

module Network_interface = struct
  type t = {
    network : string option; [@yojson.default None]
    subnetwork : string option; [@yojson.default None]
    no_external_ip_address : bool; [@key "noExternalIpAddress"] [@default false]
    nic_type : Nic_type.t option; [@key "nicType"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Network_policy = struct
  type t = {
    network_interfaces : Network_interface.t list;
        [@key "networkInterfaces"] [@default []]
  }
  [@@deriving yojson { strict = false }, make]
end

module Placement_policy = struct
  type t = {
    collocation : string option; [@yojson.default None]
    max_distance : Int64_string.t option;
        [@key "maxDistance"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Service_account = struct
  type t = {
    email : string option; [@yojson.default None]
    scopes : string list; [@default []]
  }
  [@@deriving yojson { strict = false }, make]
end

module Cloud_logging_option = struct
  type t = {
    use_generic_task_monitored_resource : bool;
        [@key "useGenericTaskMonitoredResource"] [@default false]
  }
  [@@deriving yojson { strict = false }, make]
end

module Logs_policy = struct
  type t = {
    destination : Logs_destination.t option; [@yojson.default None]
    logs_path : string option; [@key "logsPath"] [@yojson.default None]
    cloud_logging_option : Cloud_logging_option.t option;
        [@key "cloudLoggingOption"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Message = struct
  type t = {
    type_ : Message_type.t option; [@key "type"] [@yojson.default None]
    new_job_state : Job_state.t option;
        [@key "newJobState"] [@yojson.default None]
    new_task_state : Task_state.t option;
        [@key "newTaskState"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Job_notification = struct
  type t = {
    pubsub_topic : string option; [@key "pubsubTopic"] [@yojson.default None]
    message : Message.t option; [@yojson.default None]
  }
  [@@deriving yojson { strict = false }, make]
end

module Instance_status = struct
  type t = {
    machine_type : string option; [@key "machineType"] [@yojson.default None]
    provisioning_model : Provisioning_model.t option;
        [@key "provisioningModel"] [@yojson.default None]
    task_pack : Int64_string.t option; [@key "taskPack"] [@yojson.default None]
    boot_disk : Disk.t option; [@key "bootDisk"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }]
end

module Task_group_status = struct
  let counts_to_yojson = Assoc.to_yojson Int64_string.to_yojson
  let counts_of_yojson = Assoc.of_yojson Int64_string.of_yojson

  type t = {
    counts : (string * int) list;
        [@default []]
        [@to_yojson counts_to_yojson]
        [@of_yojson counts_of_yojson]
    instances : Instance_status.t list; [@default []]
  }
  [@@deriving yojson { strict = false }]

  let map_to_yojson m = Assoc.to_yojson to_yojson m
  let map_of_yojson j = Assoc.of_yojson of_yojson j
end

(* {1 Long-running operations} *)

module Rpc_status = struct
  type t = {
    code : int option; [@yojson.default None]
    message : string option; [@yojson.default None]
    details : Yojson.Safe.t list; [@default []]
  }
  [@@deriving yojson { strict = false }]
end

module Operation_metadata = struct
  type t = {
    create_time : string option; [@key "createTime"] [@yojson.default None]
    end_time : string option; [@key "endTime"] [@yojson.default None]
    target : string option; [@yojson.default None]
    verb : string option; [@yojson.default None]
    status_message : string option;
        [@key "statusMessage"] [@yojson.default None]
    requested_cancellation : bool;
        [@key "requestedCancellation"] [@default false]
    api_version : string option; [@key "apiVersion"] [@yojson.default None]
  }
  [@@deriving yojson { strict = false }]
end

module Operation = struct
  type t = {
    name : string;
    done_ : bool; [@key "done"] [@default false]
    metadata : Yojson.Safe.t option; [@yojson.default None]
    error : Rpc_status.t option; [@yojson.default None]
    response : Yojson.Safe.t option; [@yojson.default None]
  }
  [@@deriving yojson { strict = false }]

  let pp fmt t = Yojson.Safe.pretty_print fmt (to_yojson t)

  let metadata (t : t) : (Operation_metadata.t option, string) result =
    match t.metadata with
    | None -> Ok None
    | Some j -> Operation_metadata.of_yojson j |> CCResult.map CCOption.pure
end

module List_operations_response = struct
  type t = {
    operations : Operation.t list; [@default []]
    next_page_token : string option;
        [@key "nextPageToken"] [@yojson.default None]
    unreachable : string list; [@default []]
  }
  [@@deriving yojson { strict = false }]
end

[@@@warning "+39"]
