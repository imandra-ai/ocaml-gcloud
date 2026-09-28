(** {!Gcloud.Client_sig.S} backed by ezcurl (libcurl), in direct style.

    The HTTP transport is fixed; the credential source is a parameter so a
    consumer can supply its own token logic. {!Default} uses this package's
    {!Auth} discovery ({!Common.get_access_token}, {!Common.get_project_id}).

    Transport failures raise {!Http_ezcurl.Curl_error}, which the service
    functors catch and report as [`Network_error]. *)

type 'a task = 'a

module type CREDENTIALS = sig
  val get_access_token :
    scopes:string list -> unit -> (Auth.token_info, [> Error.t ]) result

  val get_project_id :
    ?project_id:string ->
    token_info:Auth.token_info ->
    unit ->
    (string, [> Error.t ]) result
end

module Make (_ : CREDENTIALS) : Gcloud.Client_sig.S with type 'a task = 'a
module Default : Gcloud.Client_sig.S with type 'a task = 'a
