module Scopes = struct
  let bigquery = "https://www.googleapis.com/auth/bigquery"
end

module Schema = struct
  [@@@warning "-39"]
  type mode = REQUIRED | NULLABLE | REPEATED
  [@@deriving show { with_path = false }]

  let mode_to_yojson mode =
    `String (show_mode mode)

  let mode_of_yojson = function
    | `String "REQUIRED" -> Ok REQUIRED
    | `String "NULLABLE" -> Ok NULLABLE
    | `String "REPEATED" -> Ok REPEATED
    | _ -> Error "mode_of_yojson"

  type bq_type =
    | BOOL
    | INTEGER
    | NUMERIC
    | FLOAT
    | STRING
    | DATE
    | TIME
    | TIMESTAMP
    | RECORD
  [@@deriving show { with_path = false }]

  let bq_type_to_yojson bq_type =
    `String (show_bq_type bq_type)

  let bq_type_of_yojson = function
    | `String "BOOL" | `String "BOOLEAN" -> Ok BOOL
    | `String "INTEGER" -> Ok INTEGER
    | `String "NUMERIC" -> Ok NUMERIC
    | `String "FLOAT" -> Ok FLOAT
    | `String "STRING" -> Ok STRING
    | `String "DATE" -> Ok DATE
    | `String "TIME" -> Ok TIME
    | `String "TIMESTAMP" -> Ok TIMESTAMP
    | `String "RECORD" -> Ok RECORD
    | j -> Error ("bq_type_of_yojson: " ^ Yojson.Safe.to_string j)

  type field =
    { name : string
    ; description : string option [@default None]
    ; mode : mode
    ; bq_type : bq_type [@key "type"]
    ; fields : field list [@default []]
    }
  [@@deriving make, yojson]
  [@@@warning "+39"]

  let bq_type_of_field field = field.bq_type
  let name_of_field field = field.name
  let description_of_field field = field.description
  let mode_of_field field = field.mode
  let fields_of_field field = field.fields
end

module Datasets = struct
  let get ?project_id ~dataset_id () : (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.bigquery] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    let project_id =
      project_id |> CCOpt.get_or ~default:token_info.project_id
    in

    Lwt.catch (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "bigquery/v2/projects/%s/datasets/%s" project_id dataset_id)
        in
        let headers =
          Cohttp.Header.of_list
            [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
        in
        Logs_lwt.debug (fun m -> m "GET %a" Uri.pp_hum uri) |> Lwt_result.ok >>= fun () ->
        Cohttp_lwt_unix.Client.get uri ~headers
        |> Lwt_result.ok
      )
      (fun e ->
         (`Network_error e)
         |> Lwt_result.fail)

    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      Cohttp_lwt.Body.to_string body |> Lwt_result.ok
    | x ->
      Error.of_response_status_code_and_body x body

  let list ?project_id () : (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.bigquery] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    let project_id =
      project_id |> CCOpt.get_or ~default:token_info.project_id
    in

    Lwt.catch (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "bigquery/v2/projects/%s/datasets" project_id)
        in
        let headers =
          Cohttp.Header.of_list
            [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
        in
        Logs_lwt.debug (fun m -> m "GET %a" Uri.pp_hum uri) |> Lwt_result.ok >>= fun () ->
        Cohttp_lwt_unix.Client.get uri ~headers
        |> Lwt_result.ok
      )
      (fun e ->
         (`Network_error e)
         |> Lwt_result.fail)

    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      Cohttp_lwt.Body.to_string body |> Lwt_result.ok
    | x ->
      Error.of_response_status_code_and_body x body

  module Tables = struct

    type table =
      { id : string;
        kind: string;
        friendlyName: string option [@default None];
        expirationTime : string option [@default None];
        creationTime : string } [@@deriving yojson{strict=false}]

    type resp =
      { tables : table list;
        kind: string;
        etag: string;
        nextPageToken: string option [@default None];
        totalItems : int } [@@deriving yojson{strict=false}]

    let list ?project_id ?max_results ?page_token ~dataset_id () : (resp, [> Error.t ]) Lwt_result.t =
      let open Lwt_result.Infix in

      Auth.get_access_token ~scopes:[Scopes.bigquery] ()
      |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
      >>= fun token_info ->

      let project_id =
        project_id |> CCOpt.get_or ~default:token_info.project_id
      in

      Lwt.catch (fun () ->
          let uri = Uri.make ()
              ~scheme:"https"
              ~host:"www.googleapis.com"
              ~path:(Printf.sprintf "bigquery/v2/projects/%s/datasets/%s/tables" project_id dataset_id)

          in
          let uri = match max_results with
            | None -> uri
            | Some max_results -> Uri.add_query_param' uri ("maxResults", string_of_int max_results)
          in
          let uri = match page_token with
            | None -> uri
            | Some page_token -> Uri.add_query_param' uri ("pageToken", page_token)
          in
          let headers =
            Cohttp.Header.of_list
              [ "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ]
          in
          Logs_lwt.debug (fun m -> m "GET %a" Uri.pp_hum uri) |> Lwt_result.ok >>= fun () ->
          Cohttp_lwt_unix.Client.get uri ~headers
          |> Lwt_result.ok
        )
        (fun e ->
           (`Network_error e)
           |> Lwt_result.fail)

      >>= fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `OK ->
        Error.parse_body_json resp_of_yojson body
      | x ->
        Error.of_response_status_code_and_body x body

  end

end

module Jobs = struct

  module Param = struct

    type numeric
    type date
    type time
    type timestamp

    type 'a struct_param =
      | FIELD : (string * 'a param') * 'b struct_param -> ( 'a * 'b ) struct_param
      | EMPTY : unit struct_param

    and _ param' =
      | P_BOOL : bool -> bool param'
      | P_INTEGER : int -> int param'
      | P_NUMERIC : string -> numeric param'
      | P_STRING : string -> string param'
      | P_DATE : string -> date param'
      | P_TIME : string -> time param'
      | P_TIMESTAMP : string -> timestamp param'
      | P_STRUCT : 'a struct_param -> 'a param'
      | P_ARRAY : Schema.bq_type * 'a param' list -> 'a param'

    type param =
      | P : 'a param' -> param

    let bool s = P_BOOL s
    let integer s = P_INTEGER s
    let numeric s = P_NUMERIC s
    let string s = P_STRING s
    let date s = P_DATE s
    let time s = P_TIME s
    let timestamp s = P_TIMESTAMP s
    let array ?(default_type=Schema.STRING) f xs = P_ARRAY (default_type, CCList.map f xs)

    let struct_ s = P_STRUCT s
    let empty_struct = EMPTY
    let with_field ~name type_ struct_ =
      FIELD ((name, type_), struct_)

    let rec struct_param_type_to_yojson : type a. a struct_param -> Yojson.Safe.t =
      fun struct_param ->
        let rec go : type a. a struct_param -> Yojson.Safe.t list =
          function
          | EMPTY -> []
          | FIELD ((name, param'), struct_param') ->
            (`Assoc
               [ ( "name", `String name )
               ; ( "type", param'_type_to_yojson param' )
               ]) :: go struct_param'
        in
        `Assoc
          [ ( "type", `String "STRUCT" )
          ; ( "structTypes", `List (go struct_param) )
          ]

    and param'_type_to_yojson : type a. a param' -> Yojson.Safe.t =
      let scalar_json value =
        `Assoc
          [ ( "type", value )]
      in
      function
      | P_BOOL _ -> scalar_json (Schema.bq_type_to_yojson Schema.BOOL)
      | P_INTEGER _ -> scalar_json (Schema.bq_type_to_yojson Schema.INTEGER)
      | P_NUMERIC _ -> scalar_json (Schema.bq_type_to_yojson Schema.NUMERIC)
      | P_STRING _ -> scalar_json (Schema.bq_type_to_yojson Schema.STRING)
      | P_DATE _ -> scalar_json (Schema.bq_type_to_yojson Schema.DATE)
      | P_TIME _ -> scalar_json (Schema.bq_type_to_yojson Schema.TIME)
      | P_TIMESTAMP _ -> scalar_json (Schema.bq_type_to_yojson Schema.TIMESTAMP)
      | P_STRUCT struct_param -> struct_param_type_to_yojson struct_param
      | P_ARRAY (default_type, array_field_params) ->
        let array_type =
          array_field_params
          |> CCList.head_opt
          |> CCOpt.map_or
            ~default:(scalar_json (Schema.bq_type_to_yojson default_type))
            param'_type_to_yojson
        in
        `Assoc
          [ ( "type", `String "ARRAY" )
          ; ( "arrayType", array_type )
          ]

    let param_type_to_yojson : param -> Yojson.Safe.t =
      function
      | P param' -> param'_type_to_yojson param'

    let rec struct_param_value_to_yojson : type a. a struct_param -> Yojson.Safe.t =
      fun struct_param ->
        let rec go : type a. a struct_param -> (string * Yojson.Safe.t) list =
          function
          | EMPTY -> []
          | FIELD ((name, param'), struct_param') ->
            ( name, param'_value_to_yojson param' ) :: go struct_param'
        in
        `Assoc
          [ ( "structValues"
            , `Assoc (go struct_param)
            )
          ]

    and param'_value_to_yojson : type a. a param' -> Yojson.Safe.t =
      let scalar_json value = `Assoc [ ( "value", value )] in
      function
      | P_BOOL b -> scalar_json (`Bool b)
      | P_INTEGER i -> scalar_json (`Int i)
      | P_NUMERIC s -> scalar_json (`String s)
      | P_STRING s -> scalar_json (`String s)
      | P_DATE d -> scalar_json (`String d)
      | P_TIME d -> scalar_json (`String d)
      | P_TIMESTAMP d -> scalar_json (`String d)
      | P_STRUCT struct_param -> struct_param_value_to_yojson struct_param
      | P_ARRAY (_, array_field_params) ->
        `Assoc
          [ ( "arrayValues", `List (CCList.map param'_value_to_yojson array_field_params) )
          ]

    let param_value_to_yojson : param -> Yojson.Safe.t =
      function
      | P param' -> param'_value_to_yojson param'

    type query_parameter =
      { name : string
      ; type_ : param
      }

    let make ~name type_ =
      { name ; type_ = P type_ }

    let query_parameter_to_yojson query_parameter =
      `Assoc
        [ ( "name", `String query_parameter.name )
        ; ( "parameterType", param_type_to_yojson query_parameter.type_ )
        ; ( "parameterValue", param_value_to_yojson query_parameter.type_ )
        ]
  end

  type parameter_mode = (* POSITIONAL | *) NAMED
  [@@deriving show { with_path = false }]

  let parameter_mode_to_yojson m =
    `String (show_parameter_mode m)

  [@@@warning "-39"]

  type data_format_options =
    { use_int64_timestamp : bool [@key "useInt64Timestamp"] }
  [@@deriving to_yojson]

  type query_request =
    { kind : string
    ; query : string
    ; use_legacy_sql : bool [@key "useLegacySql"]
    ; location : string
    ; query_parameters : Param.query_parameter list [@key "queryParameters"]
    ; parameter_mode : parameter_mode option [@key "parameterMode"] [@default None]
    ; format_options : data_format_options option [@key "formatOptions"] [@default None]
    }
  [@@deriving to_yojson]

  type job_reference =
    { job_id : string [@key "jobId"]
    ; project_id : string [@key "projectId"]
    ; location : string
    }
  [@@deriving yojson]

  type query_response_schema =
    { fields : Schema.field list}
  [@@deriving yojson]

  let map_result_l_i f xs =
    let rec go acc i = function
      | x :: xs ->
        begin match f i x with
          | Ok y -> go (y :: acc) (i+1) xs
          | Error e -> Error e
        end
      | [] -> Ok (List.rev acc)
    in
    go [] 0 xs

  type value =
    | Null
    | String of string 
    | Number of float
    | Bool of bool
    | List of value list
    | Struct of value list

  let value_of_yojson json =
    let wrapped_value decode = 
      function
      | `Assoc [("v", json)] -> decode json
      | _ -> Error {|Expected an object like { "v": value }|}
    in
    let rec aux =
      function
      | `Null -> Ok Null
      | `String s -> Ok (String s)
      | `Float f -> Ok (Number f)
      | `Bool b -> Ok (Bool b) 
      | `List jsons ->
        jsons
        |> map_result_l_i (fun i json ->
          wrapped_value aux json
          |> CCResult.map_err (fun msg -> CCFormat.sprintf "element %i: %s" i msg))
        |> CCResult.map_err (fun msg -> "in list: " ^ msg)
        |> CCResult.map (fun values -> List values )
      | `Assoc [("f", `List jsons)] ->
        jsons
        |> map_result_l_i (fun i json ->
          wrapped_value aux json
          |> CCResult.map_err (fun msg -> CCFormat.sprintf "element %i: %s" i msg))
        |> CCResult.map_err (fun msg -> "in struct: " ^ msg)
        |> CCResult.map (fun values -> Struct values )
      | _ -> Error {|expected a value|}
    in
    aux json
    |> CCResult.map_err (fun msg -> "value_of_yojson: " ^ msg)


  let rec value_to_yojson = 
    let wrapped_value enc x = `Assoc [("v", enc x)] in
    function 
    | Null -> `Null 
    | String s -> `String s
    | Number f -> `Float f
    | Bool b -> `Bool b
    | List vs -> `List (CCList.map (wrapped_value value_to_yojson) vs)
    | Struct vs -> 
      `Assoc [("f", `List (CCList.map (wrapped_value value_to_yojson) vs))]


  type query_response_row =
    { f : value list }

  let query_response_row_of_yojson json = 
    let result =
      value_of_yojson json
      |> CCResult.flat_map (function
        | Struct values -> Ok { f = values }
        | _ -> Error "expected a Struct")
    in
    result
    |> CCResult.map_err (fun msg -> "in query_response_row_of_yojson: " ^ msg)

  let query_response_row_to_yojson { f = values } = 
    value_to_yojson (Struct values)

  type query_response_data =
    { schema : query_response_schema
    ; rows : query_response_row list [@default []]
    ; page_token : string option [@key "pageToken"] [@default None]
    ; total_rows : string option [@key "totalRows"] [@default None]
    ; num_dml_affected_rows : string option [@key "numDmlAffectedRows"] [@default None]
    ; total_bytes_processed : string [@key "totalBytesProcessed"]
    ; cache_hit : bool [@key "cacheHit"]
    }
  [@@deriving of_yojson { strict = false }]

  type query_response' =
    { kind : string
    ; job_reference : job_reference [@key "jobReference"]
    ; job_complete : bool [@key "jobComplete"]
    }
  [@@deriving of_yojson { strict = false }]
  [@@@warning "+39"]

  type query_response =
    { kind : string
    ; job_reference : job_reference
    ; job_complete : query_response_data option
    }

  let query_response_of_yojson (json : Yojson.Safe.t) : (query_response, string) result =
    CCResult.(
      query_response'_of_yojson json >>= fun query_response' ->
      let query_response =
        { kind = query_response'.kind
        ; job_reference = query_response'.job_reference
        ; job_complete = None
        }
      in
      if query_response'.job_complete then
        query_response_data_of_yojson json >>= fun data ->
        return
          { query_response with
            job_complete = Some data
          }
      else
        return query_response
    )

  let query_response_data_to_yojsons (data : query_response_data) : (string * Yojson.Safe.t) list =
    List.concat
      [ [ ( "schema", query_response_schema_to_yojson data.schema )
        ; ( "rows", `List (List.map query_response_row_to_yojson data.rows) )
        ; ( "totalBytesProcessed", `String data.total_bytes_processed )
        ; ( "cacheHit", `Bool data.cache_hit )
        ]
      ; data.total_rows |> CCOpt.map_or ~default:[]
          (fun t -> [ ( "totalRows", `String t ) ])
      ; data.num_dml_affected_rows |> CCOpt.map_or ~default:[]
          (fun t -> [ ( "numDmlAffectedRows", `String t ) ])
      ; data.page_token |> CCOpt.map_or ~default:[]
          (fun t -> [ ( "pageToken", `String t ) ])
      ]

  let query_response_to_yojson (query_response : query_response) : Yojson.Safe.t =
    `Assoc
      (List.concat
         [ [ ( "kind", `String query_response.kind )
           ; ( "jobReference", job_reference_to_yojson query_response.job_reference )
           ]
         ; (match query_response.job_complete with
            | None ->
              [ ( "jobComplete", `Bool false ) ]
            | Some data ->
              ( "jobComplete", `Bool true ) :: query_response_data_to_yojsons data
           )
         ])

  let pp_query_response fmt (query_response : query_response) =
    match query_response.job_complete with
    | None ->
      Format.fprintf fmt "Response: job_id=%s job_complete=false" query_response.job_reference.job_id
    | Some data ->
      Format.fprintf fmt "Response: job_id=%s total_bytes_processed=%s cache_hit=%b rows=%d%s%s"
        query_response.job_reference.job_id
        data.total_bytes_processed
        data.cache_hit
        (CCList.length data.rows)
        (data.total_rows |> CCOpt.map_or ~default:"" (fun t -> Printf.sprintf " total_rows=%s" t))
        (data.num_dml_affected_rows |> CCOpt.map_or ~default:"" (fun t -> Printf.sprintf " num_dml_affected_rows=%s" t))

  type query_response_complete =
    { kind : string
    ; job_reference : job_reference
    ; data : query_response_data
    }

  let query_response_complete_to_yojson (query_response_complete : query_response_complete) : Yojson.Safe.t =
    `Assoc
      (List.concat
         [ [ ( "kind", `String query_response_complete.kind )
           ; ( "jobReference", job_reference_to_yojson query_response_complete.job_reference )
           ; ( "jobComplete", `Bool false )
           ]
         ; query_response_data_to_yojsons query_response_complete.data
         ])

  type ('a, 'b) decoder =  'a -> ('b, string) result
  type 'a data_decoder = (query_response_data, 'a) decoder 
  type 'a row_decoder =  (query_response_row, 'a) decoder
  type 'a value_decoder = (value, 'a) decoder

  let single_row (f : 'a row_decoder) : 'a data_decoder = fun response ->
    match response.rows with
    | [row] ->
      f row
      |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding a single_row: %s" msg)
    | _ -> Error (Printf.sprintf "Expected a single row, but got %d rows" (List.length response.rows))

  let many_rows (f : 'a row_decoder) : 'a list data_decoder = fun response ->
    response.rows
    |> map_result_l_i (fun i row ->
        f row
        |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding row %d: %s" i msg))

  let single_field (f : 'a value_decoder) : 'a row_decoder = fun row ->
    match row.f with
    | [v] -> 
      f v
      |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding a single_field: %s" msg)
    | _ ->
      Error (Printf.sprintf "Expected row with a single field, but got %d fields" (List.length row.f))

  let string : string value_decoder = function
    | String str -> Ok str
    | _ -> Error "Expected a string"

  let int : int value_decoder = fun v ->
    string v
    |> CCResult.flat_map (fun str ->
        str |> int_of_string_opt
        |> CCOpt.to_result (CCFormat.sprintf "expected an int, but got %S" str))

  let float : float value_decoder = function
    | Number f -> Ok f
    | String str ->
      str |> float_of_string_opt
      |> CCOpt.to_result (CCFormat.sprintf "expected a float, but got %S" str)
    | _ -> Error "expected a float (Number or String)"

  let bool : bool value_decoder = function
    | Bool b -> Ok b
    | String str ->
      str |> bool_of_string_opt
      |> CCOpt.to_result (CCFormat.sprintf "expected a bool, but got %S" str)
    | _ -> Error "expected a bool (Bool or String)"

  let nullable (f : 'a value_decoder) : 'a option value_decoder = function
    | Null -> Ok None
    | v -> f v |> CCResult.map CCOpt.pure

  let list (f : 'a value_decoder) : 'a list value_decoder = function
    | List vs ->
       vs |> map_result_l_i (fun i v ->
         f v |> CCResult.map_err (fun msg ->
           CCFormat.sprintf "element %i: %s" i msg))
      |> CCResult.map_err (fun msg -> CCFormat.sprintf "in list: %s" msg)
    | _ -> Error "expected a List"

  let tag msg result =
    result |> CCResult.map_err (CCFormat.sprintf "%s: %s" msg)

  let query ?project_id ?(use_legacy_sql=false) ?(params = []) ?(location = "EU")
    ?use_int64_timestamp q : (query_response, [> Error.t ]) Lwt_result.t =
    let parameter_mode =
      if use_legacy_sql || params = [] then
        None
      else
        Some NAMED
    in

    let format_options = 
      use_int64_timestamp |> CCOpt.map (fun use_int64_timestamp -> { use_int64_timestamp })
    in

    let request =
      { kind = "bigquery#queryRequest"
      ; query = q
      ; use_legacy_sql = use_legacy_sql
      ; location
      ; parameter_mode = parameter_mode
      ; query_parameters = params
      ; format_options
      }
    in

    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.bigquery] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    let project_id = project_id |> CCOpt.get_or ~default:token_info.project_id in

    Lwt.catch (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "bigquery/v2/projects/%s/queries" project_id)
        in
        let headers =
          Cohttp.Header.of_list
            [ ( "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token )
            ; ( "Content-Type", "application/json" )
            ]
        in
        let body_str =
          request
          |> query_request_to_yojson
          |> Yojson.Safe.to_string
        in
        let body =
          body_str
          |> Cohttp_lwt.Body.of_string
        in
        Logs_lwt.debug (fun m ->
            let truncate str =
              if String.length str > 1000 then CCString.sub str 0 1000 ^ "..." else str
            in
            let q_trimmed =
              q |> CCString.replace ~sub:"\n" ~by:" " |> truncate
            in
            m "Query: %s" q_trimmed)
        |> Lwt_result.ok >>= fun () ->
        Cohttp_lwt_unix.Client.post uri ~headers ~body
        |> Lwt_result.ok
      )
      (fun e ->
         (`Network_error e)
         |> Lwt_result.fail)

    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      Error.parse_body_json query_response_of_yojson body >>= fun response ->
      Logs_lwt.debug (fun m -> m "%a" pp_query_response response) |> Lwt_result.ok >>= fun () ->
      Lwt_result.return response

    | status_code ->
      Error.of_response_status_code_and_body status_code body

  let get_query_results ?(page_token) ?use_int64_timestamp (job_reference : job_reference) : (query_response, [> Error.t]) Lwt_result.t =
    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.bigquery] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    let query =
      [ page_token |> CCOpt.map (fun page_token -> ("pageToken", [page_token])) 
      ; use_int64_timestamp |> CCOpt.map (fun b -> ("formatOptions.useInt64Timestamp", [string_of_bool b]))
      ]
      |> CCList.filter_map CCFun.id
    in

    let uri =
      Uri.make ()
        ~scheme:"https"
        ~host:"www.googleapis.com"
        ~path:(Printf.sprintf "bigquery/v2/projects/%s/queries/%s" job_reference.project_id job_reference.job_id)
        ~query
    in
    let headers =
      Cohttp.Header.of_list
        [ ( "Authorization", Printf.sprintf "Bearer %s" token_info.Auth.token.access_token ) ]
    in
    Lwt.catch (fun () ->
        Cohttp_lwt_unix.Client.get uri ~headers
        |> Lwt_result.ok)
      (fun e -> Lwt_result.fail (`Network_error e))
    >>= fun (resp, body) ->

    match Cohttp.Response.status resp with
    | `OK ->
      Error.parse_body_json query_response_of_yojson body >>= fun response ->
      Logs_lwt.debug (fun m -> m "%a" pp_query_response response) |> Lwt_result.ok >>= fun () ->
      Lwt_result.return response

    | status_code ->
      Error.of_response_status_code_and_body status_code body

  let rec poll_until_complete ?(attempts = 5) (query_response : query_response) : (query_response_complete, [> Error.t]) Lwt_result.t =
    let open Lwt_result.Infix in
      match query_response.job_complete with
      | Some data ->
        Lwt.return_ok
          { kind = query_response.kind
          ; job_reference = query_response.job_reference
          ; data = data
          }
      | None ->
        if attempts <= 0 then
          Lwt.return_error (`Gcloud_retry_timeout "Big_query.Jobs.poll_until_complete: maximum number of retries reached")
        else
          Lwt_unix.sleep 0.5 |> Lwt_result.ok >>= fun () ->
          get_query_results query_response.job_reference >>= poll_until_complete ~attempts:(attempts - 1)


  let fetch_all_rows (response : query_response_complete) =
    let rec aux
        all_rows (response : query_response_complete) =
      match response.data.page_token with
      | None ->
        Lwt_result.return
          { response with
            data =
              { response.data with rows = Util.List.concat_rev all_rows }
          }
      | Some page_token ->
        let open Lwt_result.Infix in
                  get_query_results ~page_token response.job_reference
          >>= poll_until_complete
        >>= fun response2 ->
        aux (CCList.rev response2.data.rows :: all_rows) response2
    in
    aux [ CCList.rev response.data.rows ] response

end
