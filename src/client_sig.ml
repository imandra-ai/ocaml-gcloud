(** Signature for a swappable HTTP client and credential source.

    A client plays the role of Stripe's [Cohttp_client.Make (Config)]: build
    one at runtime and pass it to the service functors, or around as a
    first-class module. It is independent of the async runtime: results are
    wrapped in an abstract ['a task], which a service functor ties to its
    {!Async_task.S} argument with
    [Client : S with type 'a task = 'a Async.t]. No backend is provided here. *)
module type S = sig
  type 'a task

  val call :
    meth:Cohttp.Code.meth ->
    headers:Cohttp.Header.t ->
    ?body:string ->
    Uri.t ->
    (Cohttp.Code.status_code * string) task
  (** Perform one HTTP request and return the status code and the full
      response body. Bodies are strings on both sides: every Google JSON API
      payload the bindings deal with is small. Transport exceptions may be
      raised and are caught by the caller with {!Async_task.S.catch}. *)

  val get_access_token :
    scopes:string list -> unit -> (Auth.token_info, [> Error.t ]) result task
  (** An OAuth2 access token valid for [scopes]. Implementations are expected
      to cache and refresh; see {!Common.get_access_token} for the Lwt
      behaviour. *)

  val get_project_id :
    ?project_id:string ->
    token_info:Auth.token_info ->
    unit ->
    (string, [> Error.t ]) result task
  (** Resolve the project ID, preferring an explicitly passed one. See
      {!Common.get_project_id} for the discovery order the Lwt backend uses. *)
end
