(** {!Async_task_sig.S} backed by Lwt. *)

include Async_task_sig.S with type 'a t = 'a Lwt.t
