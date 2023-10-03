module J = Big_query.Jobs
module E = Big_query_dsl.Expression
module Q = Big_query_dsl.Query_expr

module Debug = struct
  (** Inlines query params as values for simpler debugging via the BigQuery UI in dev *)

  let inline_params ~(params : J.Param.query_parameter list) (t : Q.t) : Q.t =
    let inline_param e =
      E.Debug.map_param
        (fun name ->
          match params |> CCList.find_opt (fun p -> J.Param.name p = name) with
          | None ->
              E.(string (Format.asprintf "inlined_param(missing): %s" name))
          | Some p ->
              J.Param.param_to_expression p )
        e
    in
    Q.walk ~f_e:inline_param t
end
