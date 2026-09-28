module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
end

module Projects = struct
  module Locations = struct
    module Clusters = struct
      [@@@warning "-39"]

      type client_certificate_config = {
        issue_client_certificate : bool; [@key "issueClientCertificate"]
      }
      [@@deriving yojson]

      type master_auth = {
        username : string;
        password : string;
        client_certificate_config : client_certificate_config option;
            [@key "clientCertificateConfig"] [@default None]
        cluster_ca_certificate : string; [@key "clusterCaCertificate"]
        client_certificate : string; [@key "clientCertificate"]
        client_key : string; [@key "clientKey"]
      }
      [@@deriving yojson]

      type t = {
        name : string;
        description : string;
        master_auth : master_auth; [@key "masterAuth"]
      }
      [@@deriving yojson { strict = false }]

      [@@@warning "+39"]
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

  module Projects = struct
    module Locations = struct
      module Clusters = struct
        include Projects.Locations.Clusters

        let get ?project_id ~(location : string) ~(cluster : string) () :
            (t, [> Error.t ]) result task =
          let open R.Infix in
          Client.get_access_token ~scopes:[ Scopes.cloud_platform ] ()
          >>= fun token_info ->
          Client.get_project_id ?project_id ~token_info () >>= fun project_id ->
          let uri =
            Uri.make () ~scheme:"https" ~host:"container.googleapis.com"
              ~path:
                (Printf.sprintf "v1beta1/projects/%s/locations/%s/clusters/%s"
                   project_id location cluster)
          in
          let headers = Cohttp.Header.of_list [ Req.bearer token_info ] in
          Req.call ~meth:`GET ~headers uri >>= fun (status, body) ->
          match status with
          | `OK -> R.lift (Error.parse_body_json of_yojson body)
          | status_code ->
              R.lift (Error.of_response_status_code_and_body status_code body)
      end
    end
  end
end
