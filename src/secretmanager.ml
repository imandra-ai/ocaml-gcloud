let ok = Lwt_result.ok

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

        (** Accesses a SecretVersion. This call returns the secret data.
          [projects/*/secrets/*/versions/latest] is an alias to the most recently created [SecretVersion].

          @param name Required. The resource name of the SecretVersion in the format [projects/*/secrets/*/versions/*].[projects/*/secrets/*/versions/latest] is an alias to the most recently created [SecretVersion].
       *)

        let access ~(name : string) =
          let open Lwt_result.Syntax in
          let* token_info =
            Common.get_access_token ~scopes:[ Scopes.cloud_platform ] ()
          in
          let open Lwt_result.Syntax in
          let* resp, body =
            Lwt.catch
              (fun () ->
                let uri =
                  Uri.make () ~scheme:"https"
                    ~host:"secretmanager.googleapis.com"
                    ~path:(Printf.sprintf "v1/%s:access" name)
                in
                let headers =
                  Cohttp.Header.of_list
                    [
                      ( "Authorization",
                        Printf.sprintf "Bearer %s" token_info.token.access_token
                      );
                    ]
                in
                let open Lwt.Infix in
                Cohttp_lwt_unix.Client.get uri ~headers
                >>= Util.consume_body |> ok)
              (fun e -> Lwt_result.fail (`Network_error e))
          in

          match Cohttp.Response.status resp with
          | `OK -> Error.parse_body_json response_of_yojson body |> Lwt.return
          | status_code ->
              Error.of_response_status_code_and_body status_code body
      end
    end
  end
end
