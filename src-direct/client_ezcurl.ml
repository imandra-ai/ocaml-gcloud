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

module Make (C : CREDENTIALS) = struct
  type 'a task = 'a

  let call ~meth ~headers ?body uri = Http_ezcurl.call ~meth ~headers ?body uri
  let get_access_token = C.get_access_token
  let get_project_id = C.get_project_id
end

module Default = Make (struct
  let get_access_token ~scopes () = Common.get_access_token ~scopes ()
  let get_project_id = Common.get_project_id
end)
