type src_level = { src_prefix : string; level : Logs.level option }
type t = src_level list

let pp_src_level fmt { src_prefix; level } =
  let open CCFormat in
  fprintf fmt "%s:%a" src_prefix (some Logs.pp_level) level

(** Parse a string of log levels, e.g. from a --log CLI arg or the OCAML_GCLOUD_LOG
    environment variable.

    Syntax: ["[SRC_PREFIX:]LEVEL[ [SRC_PREFIX:]LEVEL]..."]

    Example: ["warn x:info x.y:debug"]

    Means:
    - set all sources to the [WARN] level
    - set sources with prefix ["x"] to the [INFO] level
    - set sources with prefix ["x.y"] to the [DEBUG] level
 *)
let src_levels_of_string str : t option =
  str |> CCString.split_on_char ' '
  |> CCList.map (fun str ->
         let open CCOption.Infix in
         let* src_prefix, level_str =
           match CCString.split_on_char ':' str with
           | [ src_prefix; level_str ] -> Some (src_prefix, level_str)
           | [ level_str ] -> Some ("", level_str)
           | _ -> None
         in
         let* level = Logs.level_of_string level_str |> CCOption.of_result in
         Some { src_prefix; level })
  |> CCList.all_some

let setup (levels : t) =
  Logs.set_reporter (Logs_fmt.reporter ());
  levels
  |> CCList.iter (fun { src_prefix; level } ->
         Logs.Src.list ()
         |> CCList.filter (fun src ->
                CCString.prefix ~pre:src_prefix (Logs.Src.name src))
         |> CCList.iter (fun src -> Logs.Src.set_level src level))

let parser str =
  match src_levels_of_string str with
  | Some x -> `Ok x
  | None -> `Error "Invalid src_levels"

let conv : t Cmdliner.Arg.conv =
  (parser, CCFormat.(list ~sep:(return " ") pp_src_level))
