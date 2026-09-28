(** Signature for a swappable async runtime.

    Service bindings can be functorised over {!S} so the same code runs on
    Lwt, or in direct style on an effects-based scheduler (picos, moonpool,
    ...). No backend is provided here.

    For a direct-style scheduler the instantiation is the identity:
    [type 'a t = 'a], [bind x f = f x], [catch] is [try ... with] and [sleep]
    is the scheduler's sleep. *)
module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  (** [catch f handler] runs [f ()] and routes any exception it raises to
      [handler]. Used to turn transport failures into [`Network_error]. *)

  val sleep : float -> unit t
  (** Sleep for the given number of seconds. Used by polling helpers. *)
end
