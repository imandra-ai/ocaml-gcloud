(** {!Async_task_sig.S} backed by Lwt. *)

include Gcloud.Async_task_sig.S with type 'a t = 'a Lwt.t
