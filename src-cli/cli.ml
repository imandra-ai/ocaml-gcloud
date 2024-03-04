open Cmdliner

let src = Logs.Src.create "ocaml-gcloud.cli"

module Log = (val Logs.src_log src : Logs.LOG)

let help_secs = [ `S Manpage.s_common_options ]
let sdocs = Manpage.s_common_options

(* Options common to all commands *)

type copts = { project_id : string option; log_levels : Cli_logs.t }

let copts ~project_id ~log_levels = { project_id; log_levels }

let copts_t =
  let docs = Manpage.s_common_options in
  let project_id =
    let doc = "cloud project ID" in
    let env = Cmd.Env.info "PROJECT_ID" in
    Arg.(
      value
      & opt (some string) None
      & info [ "project-id" ] ~docv:"PROJECT_ID" ~docs ~doc ~env)
  in
  let log_levels =
    let doc = "Log levels" in
    let env = Cmd.Env.info "OCAML_GCLOUD_LOG" in
    Arg.(
      value & opt Cli_logs.conv []
      & info [ "log" ] ~docv:"[src:]level[ [src:]level]..." ~doc ~env)
  in
  Term.(
    const (fun project_id log_levels -> copts ~project_id ~log_levels)
    $ project_id $ log_levels)

let print_result ~pp = function
  | Ok x -> Log.app (fun m -> m "%a" pp x)
  | Error e -> Log.err (fun m -> m "%a" Gcloud.Error.pp e)

let main ~copts ~pp f =
  Cli_logs.setup copts.log_levels;
  let lwt =
    let open Lwt.Syntax in
    let* result = f () in
    let () = print_result ~pp result in
    Lwt.return ()
  in
  Lwt_main.run lwt

module Secrets = struct
  module Versions = struct
    module Access = struct
      let access ~copts ~secret version =
        let open Gcloud in
        let f () =
          let open Lwt_result.Syntax in
          let* token_info =
            Common.get_access_token ~scopes:[ Auth.Scopes.cloud_platform ] ()
          in
          let* project_id =
            Common.get_project_id ?project_id:copts.project_id ~token_info ()
          in
          let* s =
            Secretmanager.V1.Projects.Secrets.Versions.access
              ~name:
                (Format.asprintf "projects/%s/secrets/%s/versions/%s" project_id
                   secret version)
          in
          Lwt.return (Base64.decode s.payload.data)
        in
        main ~copts ~pp:(fun fmt s -> CCFormat.fprintf fmt "%s" s) f

      let cmd =
        let secret =
          let doc = "Name of the secret" in
          Arg.(
            required
            & opt (some string) None
            & info [ "secret" ] ~docv:"SECRET_NAME" ~doc)
        in
        let version =
          let doc =
            "Version of the secret. Use 'latest' to get the most recent version"
          in
          Arg.(
            required
            & pos 0 (some string) None
            & info [] ~doc ~docv:"SECRET_VERSION")
        in
        let doc = "Access a secret version" in
        let man =
          [
            `S Manpage.s_description;
            `P "Access a secret version";
            `Blocks help_secs;
          ]
        in
        let info = Cmd.info "access" ~doc ~sdocs ~man in
        Cmd.v info
          Term.(
            const (fun copts secret version -> access ~copts ~secret version)
            $ copts_t $ secret $ version)
    end

    let cmd =
      let doc = "Interact with secrets-manager" in
      let info = Cmd.info "versions" ~doc in
      Cmd.group info [ Access.cmd ]
  end

  let cmd =
    let doc = "Interact with secrets-manager" in
    let info = Cmd.info "secrets" ~doc in
    Cmd.group info [ Versions.cmd ]
end

let main_cmd =
  let doc = "ocaml-gcloud cli" in
  let man = help_secs in
  let info = Cmd.info "ocaml-gcloud" ~version:"%%VERSION%%" ~doc ~sdocs ~man in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  Cmd.group info ~default [ Secrets.cmd ]

let top () = exit (Cmd.eval main_cmd)
