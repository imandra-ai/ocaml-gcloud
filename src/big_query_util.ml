module J = Big_query.Jobs
module E = Gcloud_shared.Big_query_dsl.Expression
module Q = Gcloud_shared.Big_query_dsl.Query_expr

module Debug = struct
  (** Inlines query params as values for simpler debugging via the BigQuery UI in dev *)

  let inline_params ~(params : J.Param.query_parameter list) (t : Q.t) : Q.t =
    let inline_param e =
      E.Debug.map_param
        (fun name ->
          match params |> CCList.find_opt (fun p -> J.Param.name p = name) with
          | None ->
              E.(param name)
          | Some p ->
              J.Param.param_to_expression p )
        e
    in
    Q.walk_expression inline_param t
end
