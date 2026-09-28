module Scopes : sig
  val pubsub : string
end

module Subscriptions : sig
  type message = { data : string; message_id : string; publish_time : string }
  type received_message = { ack_id : string; message : message }
  type received_messages = { received_messages : received_message list }

  val log_src_pull : Logs.Src.t
end

module Make
    (Async : Async_task_sig.S)
    (_ : Client_sig.S with type 'a task = 'a Async.t) : sig
  type 'a task = 'a Async.t

  module Scopes : sig
    val pubsub : string
  end

  module Subscriptions : sig
    include module type of struct
      include Subscriptions
    end

    val acknowledge :
      ?project_id:string ->
      subscription_id:string ->
      ids:string list ->
      unit ->
      (unit, [> Error.t ]) result task

    val pull :
      ?project_id:string ->
      subscription_id:string ->
      max_messages:int ->
      ?return_immediately:bool ->
      unit ->
      (received_messages, [> Error.t ]) result task
  end
end
