module Scopes = struct
  let cloudkms = "https://www.googleapis.com/auth/cloudkms"
end

module V1 = struct
  module Locations = struct
    module KeyRings = struct
      module CryptoKeys = struct
        let decrypt ?project_id ~location ~key_ring ~crypto_key ciphertext :
            (string, [> Error.t ]) Lwt_result.t =
          let open Lwt_result.Infix in
          Common.get_access_token ~scopes:[ Scopes.cloudkms ] ()
          >>= fun token_info ->
          Common.get_project_id ?project_id ~token_info () >>= fun project_id ->
          Lwt.catch
            (fun () ->
              let uri =
                Uri.make () ~scheme:"https" ~host:"cloudkms.googleapis.com"
                  ~path:
                    (Printf.sprintf
                       "v1/projects/%s/locations/%s/keyRings/%s/cryptoKeys/%s:decrypt"
                       project_id location key_ring crypto_key)
              in
              let b64_encoded =
                Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet ciphertext
              in
              let body =
                `Assoc [ ("ciphertext", `String b64_encoded) ]
                |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
              in
              let headers =
                Cohttp.Header.of_list
                  [
                    ( "Authorization",
                      Printf.sprintf "Bearer %s"
                        token_info.Auth.token.access_token );
                  ]
              in
              Cohttp_lwt_unix.Client.post uri ~headers ~body |> Lwt_result.ok)
            (fun e -> `Network_error e |> Lwt_result.fail)
          >>= fun (resp, body) ->
          match Cohttp.Response.status resp with
          | `OK ->
              Error.parse_body_json
                (function
                  | `Assoc [ ("plaintext", `String plaintext) ] -> (
                      try
                        Ok
                          (Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet
                             plaintext)
                      with Not_found ->
                        Error "Could not base64-decode the plaintext")
                  | _ -> Error "Expected an object with field 'plaintext'")
                body
          | x -> Error.of_response_status_code_and_body x body
      end
    end
  end
end
