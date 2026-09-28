module Scopes = struct
  let cloudkms = "https://www.googleapis.com/auth/cloudkms"
end

module Make
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  type 'a task = 'a Async.t

  module R = Async_task_result.Make (Async)
  module Req = Request.Make (Async) (Client)
  module Scopes = Scopes

  module V1 = struct
    module Locations = struct
      module KeyRings = struct
        module CryptoKeys = struct
          let call ?project_id ~location ~key_ring ~crypto_key ~action ~field
              data parse : (string, [> Error.t ]) result task =
            let open R.Infix in
            Client.get_access_token ~scopes:[ Scopes.cloudkms ] ()
            >>= fun token_info ->
            Client.get_project_id ?project_id ~token_info ()
            >>= fun project_id ->
            let uri =
              Uri.make () ~scheme:"https" ~host:"cloudkms.googleapis.com"
                ~path:
                  (Printf.sprintf
                     "v1/projects/%s/locations/%s/keyRings/%s/cryptoKeys/%s:%s"
                     project_id location key_ring crypto_key action)
            in
            let b64_encoded =
              Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet data
            in
            let body =
              `Assoc [ (field, `String b64_encoded) ] |> Yojson.Safe.to_string
            in
            let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
            Req.call ~meth:`POST ~headers ~body uri >>= fun (status, body) ->
            match status with
            | `OK -> R.lift (Error.parse_body_json parse body)
            | x -> R.lift (Error.of_response_status_code_and_body x body)

          let decrypt ?project_id ~location ~key_ring ~crypto_key ciphertext :
              (string, [> Error.t ]) result task =
            call ?project_id ~location ~key_ring ~crypto_key ~action:"decrypt"
              ~field:"ciphertext" ciphertext (function
              | `Assoc [ ("plaintext", `String plaintext) ] -> (
                  try
                    Ok
                      (Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet
                         plaintext)
                  with Not_found | Invalid_argument _ ->
                    Error "Could not base64-decode the plaintext")
              | _ -> Error "Expected an object with field 'plaintext'")

          let encrypt ?project_id ~location ~key_ring ~crypto_key plaintext :
              (string, [> Error.t ]) result task =
            call ?project_id ~location ~key_ring ~crypto_key ~action:"encrypt"
              ~field:"plaintext" plaintext (function
              | `Assoc fields -> (
                  match List.assoc_opt "ciphertext" fields with
                  | Some (`String ciphertext) -> (
                      try Ok (Base64.decode_exn ciphertext)
                      with Invalid_argument _ ->
                        Error "Could not base64-decode the ciphertext")
                  | _ -> Error "Expected an object with field 'ciphertext'")
              | _ -> Error "Expected an object with field 'ciphertext'")
        end
      end
    end
  end
end
