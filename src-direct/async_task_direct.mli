(** {!Gcloud.Async_task_sig.S} in direct style: a task is just its value.

    [sleep] blocks the current thread with [Unix.sleepf]. That is correct on a
    thread pool such as moonpool (whose 0.11 workers do not support picos
    timers), at the cost of one worker for the duration. On a picos scheduler
    with timer support, prefer {!Async_task_picos} from [gcloud-direct.picos],
    which suspends the fiber instead. *)

include Gcloud.Async_task_sig.S with type 'a t = 'a
