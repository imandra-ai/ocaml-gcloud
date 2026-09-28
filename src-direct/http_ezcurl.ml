(** Blocking HTTP via ezcurl/libcurl. [Curl.perform] releases the OCaml
    runtime lock, so other domains and threads keep running during a request. *)

exception Curl_error of Curl.curlCode * string

let global_init = lazy (Curl.global_init Curl.CURLINIT_GLOBALALL)

let http ?(timeout_s : float option) ~(meth : Cohttp.Code.meth)
    ~(headers : Cohttp.Header.t) ?(body : string option) (uri : Uri.t) :
    Ezcurl.response =
  Lazy.force global_init;
  let with_body = `String (CCOption.get_or ~default:"" body) in
  (* ezcurl only feeds a request body for PUT and POST. Any other method with
     a body is sent POST-style with the method name overridden. *)
  let ezmeth, content, custom_method =
    match (meth, body) with
    | `GET, None -> (Ezcurl.GET, None, None)
    | `HEAD, None -> (Ezcurl.HEAD, None, None)
    | `DELETE, None -> (Ezcurl.DELETE, None, None)
    | `OPTIONS, None -> (Ezcurl.OPTIONS, None, None)
    | `TRACE, None -> (Ezcurl.TRACE, None, None)
    | `CONNECT, None -> (Ezcurl.CONNECT, None, None)
    | `PUT, _ -> (Ezcurl.PUT, Some with_body, None)
    | `POST, _ -> (Ezcurl.POST [], Some with_body, None)
    | ( ( `PATCH | `Other _ | `GET | `HEAD | `DELETE | `OPTIONS | `TRACE
        | `CONNECT ),
        _ ) ->
        ( Ezcurl.POST [],
          Some with_body,
          Some (Cohttp.Code.string_of_method meth) )
  in
  let set_opts c =
    Curl.set_nosignal c true;
    timeout_s
    |> CCOption.iter (fun t ->
           let ms = int_of_float (t *. 1000.) in
           Curl.set_timeoutms c ms;
           Curl.set_connecttimeoutms c ms);
    custom_method |> CCOption.iter (Curl.set_customrequest c)
  in
  let client = Ezcurl.make ~set_opts () in
  let result =
    Fun.protect
      ~finally:(fun () -> Ezcurl.delete client)
      (fun () ->
        Ezcurl.http ~client
          ~headers:(Cohttp.Header.to_list headers)
          ?content ~url:(Uri.to_string uri) ~meth:ezmeth ())
  in
  match result with
  | Ok response -> response
  | Error (code, msg) -> raise (Curl_error (code, msg))

let call ?timeout_s ~meth ~headers ?body uri : Cohttp.Code.status_code * string
    =
  let r = http ?timeout_s ~meth ~headers ?body uri in
  (Cohttp.Code.status_of_code r.Ezcurl.code, r.Ezcurl.body)
