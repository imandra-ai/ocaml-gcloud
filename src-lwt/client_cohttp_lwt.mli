(** {!Client_sig.S} backed by [cohttp-lwt-unix], using this library's usual
    credential discovery ({!Common.get_access_token},
    {!Common.get_project_id}). Pair it with {!Async_task_lwt}. *)

include Gcloud.Client_sig.S with type 'a task = 'a Lwt.t
