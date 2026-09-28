let src = Logs.Src.create "gcloud.direct.common"

module Log = (val Logs.src_log src)

module Cloud_sdk = struct
  let get_project_id () : (string * string) option =
    try
      let active_config_name =
        CCIO.with_in Auth.Paths.active_config CCIO.read_all |> String.trim
      in
      let active_config_path = Auth.Paths.config ~active_config_name in
      let active_config = CCIO.with_in active_config_path CCIO.read_all in
      CCString.lines active_config
      |> CCList.find_map (CCString.Split.left ~by:"project = ")
      |> CCOption.map (fun (_pre, v) ->
             (v, Format.asprintf "Cloud SDK Config: %s" active_config_path))
    with Sys_error _ -> None
end

let project_id_of_credentials (credentials : Auth.credentials) : string option =
  match credentials with
  | Service_account { project_id; _ } | GCE_metadata { project_id } ->
      Some project_id
  | Authorized_user _ | External_account _ -> None

(** [project_id] optional arg is a convenience where project_id is optionally
    available for the caller, e.g. a CLI entrypoint where --project-id X may or
    may not have been passed *)
let get_project_id ?project_id ~(token_info : Auth.token_info) () :
    (string, [> Error.t ]) result =
  let m label x = CCOption.map (fun pid -> (pid, label)) x in
  match
    CCOption.choice
      [
        project_id |> m "Explicitly passed";
        Sys.getenv_opt Auth.Environment_vars.google_project_id
        |> m "Environment variable";
        project_id_of_credentials token_info.credentials |> m "Credentials";
        Cloud_sdk.get_project_id ();
      ]
  with
  | Some (project_id, label) ->
      Log.debug (fun m -> m "Using project_id from: %s" label);
      Ok project_id
  | None -> Error `No_project_id

let get_access_token ?scopes () : (Auth.token_info, [> Error.t ]) result =
  Auth.get_access_token ?scopes ()
  |> CCResult.map_err (fun e -> `Gcloud_auth_error e)
