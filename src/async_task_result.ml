(** [('a, 'e) result] inside an {!Async_task_sig.S} task, with the usual
    combinators. Mirrors the subset of [Lwt_result] the bindings use. *)
module Make (Async : Async_task_sig.S) = struct
  type ('a, 'e) t = ('a, 'e) result Async.t

  let return (x : 'a) : ('a, 'e) t = Async.return (Ok x)
  let fail (e : 'e) : ('a, 'e) t = Async.return (Error e)
  let lift (r : ('a, 'e) result) : ('a, 'e) t = Async.return r

  let ok (t : 'a Async.t) : ('a, 'e) t =
    Async.bind t (fun x -> Async.return (Ok x))

  let bind (t : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    Async.bind t (function Ok x -> f x | Error e -> Async.return (Error e))

  let map (f : 'a -> 'b) (t : ('a, 'e) t) : ('b, 'e) t =
    bind t (fun x -> return (f x))

  let map_error (f : 'e -> 'f) (t : ('a, 'e) t) : ('a, 'f) t =
    Async.bind t (function Ok x -> return x | Error e -> fail (f e))

  module Infix = struct
    let ( >>= ) = bind
    let ( >|= ) t f = map f t
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) t f = map f t
  end
end
