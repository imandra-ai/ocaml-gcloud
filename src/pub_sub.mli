module Scopes : sig
  val pubsub : string
end

module Subscriptions : sig
  val acknowledge : ?project_id:string -> subscription_id:string -> ids:string list -> (unit, [> Error.t ]) Lwt_result.t
end
