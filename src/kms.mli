module Scopes : sig
  val cloudkms : string
end

module Make
    (Async : Async_task_sig.S)
    (_ : Client_sig.S with type 'a task = 'a Async.t) : sig
  type 'a task = 'a Async.t

  module Scopes : sig
    val cloudkms : string
  end

  module V1 : sig
    module Locations : sig
      module KeyRings : sig
        module CryptoKeys : sig
          val decrypt :
            ?project_id:string ->
            location:string ->
            key_ring:string ->
            crypto_key:string ->
            string ->
            (string, [> Error.t ]) result task

          val encrypt :
            ?project_id:string ->
            location:string ->
            key_ring:string ->
            crypto_key:string ->
            string ->
            (string, [> Error.t ]) result task
        end
      end
    end
  end
end
