(** Shared HTTP plumbing for service functors: run one request through the
    client, turning transport exceptions into [`Network_error]. *)
module Make
    (Async : Async_task_sig.S)
    (Client : Client_sig.S with type 'a task = 'a Async.t) =
struct
  module R = Async_task_result.Make (Async)

  let src = Logs.Src.create "gcloud.request"

  module Log = (val Logs.src_log src : Logs.LOG)

  let bearer (token_info : Auth.token_info) : string * string =
    ("Authorization", Printf.sprintf "Bearer %s" token_info.token.access_token)

  let call ~(meth : Cohttp.Code.meth) ~(headers : Cohttp.Header.t)
      ?(body : string option) (uri : Uri.t) :
      (Cohttp.Code.status_code * string, [> Error.t ]) result Async.t =
    Log.debug (fun m ->
        m "%s %a" (Cohttp.Code.string_of_method meth) Uri.pp_hum uri);
    Async.catch
      (fun () -> R.ok (Client.call ~meth ~headers ?body uri))
      (fun e -> R.fail (`Network_error e))
end
