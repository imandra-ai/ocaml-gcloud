(** {!Gcloud.Async_task_sig.S} in direct style with a fiber-suspending
    [sleep] ([Picos_std_structured.Control.sleep]).

    Requires a picos scheduler that implements [cancel_after] (the picos_mux
    schedulers do). Moonpool 0.11 does not, and raises
    [Failure "Moonpool: cancel_after is not supported."] from [sleep]; use
    {!Async_task_direct} there. *)

include Gcloud.Async_task_sig.S with type 'a t = 'a
