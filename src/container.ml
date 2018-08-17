module Scopes = struct
  let cloud_platform = "https://www.googleapis.com/auth/cloud-platform"
end

module Projects = struct
  module Locations = struct
    module Clusters = struct

      [@@@warning "-39"]
      type client_certificate_config =
        { issue_client_certificate : bool [@key "issueClientCertificate"]
        } [@@deriving yojson]

      type master_auth =
        { username : string
        ; password : string
        ; client_certificate_config : client_certificate_config option [@key "clientCertificateConfig"] [@default None]
        ; cluster_ca_certificate : string [@key "clusterCaCertificate"]
        ; client_certificate : string [@key "clientCertificate"]
        ; client_key : string [@key "clientKey"]
        } [@@deriving yojson]

      type t =
        { name : string
        ; description : string
        ; master_auth : master_auth [@key "masterAuth"]
        } [@@deriving yojson{strict=false}]
      [@@@warning "+39"]

      let get ~(project : string) ~(location : string) ~(cluster : string) : (t, [> Error.t]) Lwt_result.t =
        let open Lwt_result.Infix in

        Auth.get_access_token ~scopes:[Scopes.cloud_platform] ()
        |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)

        >>= fun token_info ->
        Lwt.catch
          (fun () ->
             let uri = Uri.make ()
                 ~scheme:"https"
                 ~host:"container.googleapis.com"
                 ~path:(Printf.sprintf "v1beta1/projects/%s/locations/%s/clusters/%s" project location cluster)
             in
             let headers =
               Cohttp.Header.of_list
                 [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
             in
             (Cohttp_lwt_unix.Client.get uri ~headers:headers)
             |> Lwt_result.ok
          )
          (fun e -> Lwt_result.fail (`Network_error e))
        >>= fun (resp, body) ->
        match Cohttp.Response.status resp with
        | `OK ->
          Error.parse_body_json of_yojson body
        | status_code ->
          Error.of_response_status_code_and_body status_code body

    end
  end
end
