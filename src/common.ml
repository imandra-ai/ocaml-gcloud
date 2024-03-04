module Cloud_sdk = struct
  let get_project_id () =
    let open Lwt.Syntax in
    let* active_config_name =
      Lwt_io.(
        with_file ~mode:input Auth.Paths.active_config (fun ic -> read_line ic))
    in
    let* active_config =
      Lwt_io.(
        with_file ~mode:input
          Auth.Paths.(config ~active_config_name)
          (fun ic -> read ic))
    in
    CCString.lines active_config
    |> CCList.find_map (CCString.Split.left ~by:"project = ")
    |> CCOption.map (fun (_pre, v) -> v)
    |> Lwt_result.return
end

let project_id_of_credentials (credentials : Auth.credentials) : string option =
  match credentials with
  | Service_account { project_id; _ } | GCE_metadata { project_id } ->
      Some project_id
  | Authorized_user _ | External_account _ -> None

(** [project_id] optional arg is a convenience where project_id is optionally
    available for the caller, e.g. a CLI entrypoint where --project-id X may or may
    not have been passed *)
let get_project_id ?project_id ~token_info () =
  let open Lwt_result.Syntax in
  match
    CCOption.choice
      [
        project_id;
        Sys.getenv_opt Auth.Environment_vars.google_project_id;
        project_id_of_credentials token_info.Auth.credentials;
      ]
  with
  | Some project_id -> Lwt_result.return project_id
  | None -> (
      let* pid = Cloud_sdk.get_project_id () in
      match pid with
      | Some project_id -> Lwt_result.return project_id
      | None -> Lwt_result.fail `No_project_id)

let get_access_token ?scopes () : (Auth.token_info, [> Error.t ]) Lwt_result.t =
  Auth.get_access_token ?scopes ()
  |> Lwt_result.map_error (fun e -> `Gcloud_auth_error e)
