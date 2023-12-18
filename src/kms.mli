module Scopes : sig
  val cloudkms : string
end

module V1 : sig
  module Locations : sig
    module KeyRings : sig
      module CryptoKeys : sig
        val decrypt :
          location:string ->
          key_ring:string ->
          crypto_key:string ->
          string ->
          (string, [> Error.t ]) result Lwt.t
      end
    end
  end
end
