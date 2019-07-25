module Scopes : sig
  val pubsub : string
end

module Subscriptions : sig
  val acknowledge : ?project_id:string -> subscription_id:string -> ids:string list -> unit -> (unit, [> Error.t ]) Lwt_result.t

  type message = {
    data : string;
    message_id : string;
    publish_time : string
  }

  type received_message = {
    ack_id : string;
    message : message
  }

  type received_messages = {
    received_messages : received_message list
  }

  val pull : ?project_id:string -> subscription_id:string -> max_messages:int -> ?return_immediately:bool -> unit -> (received_messages, [> Error.t ]) Lwt_result.t

end
