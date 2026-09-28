module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
end

module V1 = struct
  module Projects = struct
    module Secrets = struct
      module Versions = struct
        type secret_payload = {
          data : string;
          data_crc32c : string option; [@default None] [@key "dataCrc32c"]
        }
        [@@deriving yojson]

        type response = { name : string; payload : secret_payload }
        [@@deriving yojson]
      end
    end
  end
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
    module Projects = struct
      module Secrets = struct
        module Versions = struct
          include V1.Projects.Secrets.Versions

          (** Accesses a SecretVersion. This call returns the secret data.
              [projects/*/secrets/*/versions/latest] is an alias to the most
              recently created [SecretVersion].

              @param name
                Required. The resource name of the SecretVersion in the format
                [projects/*/secrets/*/versions/*].
                [projects/*/secrets/*/versions/latest] is an alias to the most
                recently created [SecretVersion]. *)
          let access ~(name : string) : (response, [> Error.t ]) result task =
            let open R.Infix in
            Client.get_access_token ~scopes:[ Scopes.cloud_platform ] ()
            >>= fun token_info ->
            let uri =
              Uri.make () ~scheme:"https" ~host:"secretmanager.googleapis.com"
                ~path:(Printf.sprintf "v1/%s:access" name)
            in
            let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
            Req.call ~meth:`GET ~headers uri >>= fun (status, body) ->
            match status with
            | `OK -> R.lift (Error.parse_body_json response_of_yojson body)
            | status_code ->
                R.lift (Error.of_response_status_code_and_body status_code body)
        end
      end
    end
  end
end
