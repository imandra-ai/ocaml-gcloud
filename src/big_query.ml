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
    | STRING
    | DATE
    | TIME
    | TIMESTAMP
    | RECORD
  [@@deriving show { with_path = false }]

  let bq_type_to_yojson bq_type =
    `String (show_bq_type bq_type)

  let bq_type_of_yojson = function
    | `String "BOOL" -> Ok BOOL
    | `String "INTEGER" -> Ok INTEGER
    | `String "NUMERIC" -> Ok NUMERIC
    | `String "STRING" -> Ok STRING
    | `String "DATE" -> Ok DATE
    | `String "TIME" -> Ok TIME
    | `String "TIMESTAMP" -> Ok TIMESTAMP
    | `String "RECORD" -> Ok RECORD
    | _ -> Error "bq_type_of_yojson"

  type field =
    { name : string
    ; description : string option [@default None]
    ; mode : mode
    ; bq_type : bq_type [@key "type"]
    ; fields : field list [@default []]
    }
  [@@deriving make, yojson]
  [@@@warning "+39"]
end

module Datasets = struct
  let list () : (string, [> Error.t ]) Lwt_result.t =
    let open Lwt_result.Infix in

    Auth.get_access_token ~scopes:[Scopes.bigquery] ()
    |> Lwt_result.map_err (fun e -> `Gcloud_auth_error e)
    >>= fun token_info ->

    Lwt.catch (fun () ->
        let uri = Uri.make ()
            ~scheme:"https"
            ~host:"www.googleapis.com"
            ~path:(Printf.sprintf "bigquery/v2/projects/%s/datasets" token_info.project_id)
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
end

module Jobs = struct
  [@@@warning "-39"]
  type job_reference =
    { jobId : string
    ; projectId : string
    ; location : string
    }
  [@@deriving yojson]

  type query_parameter_type =
    { type_ : Schema.bq_type [@key "type"]
    }
  [@@deriving yojson]

  type query_parameter_value =
    { value : string
    }
  [@@deriving yojson]

  type query_parameter =
    { name : string
    ; parameterType : query_parameter_type
    ; parameterValue : query_parameter_value
    }
  [@@deriving yojson]

  let param ~name ~type_ ~value =
    { name
    ; parameterType = { type_ }
    ; parameterValue = { value }
    }

  type parameter_mode = POSITIONAL | NAMED
  [@@deriving show { with_path = false }]

  let parameter_mode_to_yojson m =
    `String (show_parameter_mode m)

  let parameter_mode_of_yojson = function
    | `String "POSITIONAL" -> Ok POSITIONAL
    | `String "NAMED" -> Ok NAMED
    | _ -> Error "parameter_mode_of_yojson"

  type query_request =
    { kind : string
    ; query : string
    ; useLegacySql : bool
    ; location : string
    ; queryParameters : query_parameter list
    ; parameterMode : parameter_mode option
    }
  [@@deriving yojson]

  type query_response_schema =
    { fields : Schema.field list}
  [@@deriving yojson]

  type query_response_field =
    { v : string }
  [@@deriving yojson]

  type query_response_row =
    { f : query_response_field list}
  [@@deriving yojson]

  type query_response =
    { kind : string
    ; schema : query_response_schema
    ; rows : query_response_row list [@default []]
    ; pageToken : string option [@default None]
    ; totalRows : string
    ; jobReference : job_reference
    ; jobComplete : bool
    ; totalBytesProcessed : string
    ; cacheHit : bool
    }
  [@@deriving yojson { strict = false }]
  [@@@warning "+39"]

  let map_result_l_i f xs =
    let rec go i = function
      | x :: xs ->
        begin match f i x with
          | Ok y -> begin match go (i+1) xs with
              | Ok ys -> Ok (y :: ys)
              | Error e -> Error e
            end
          | Error e -> Error e
        end
      | [] -> Ok []
    in
    go 0 xs

  let single_row (f : query_response_row -> ('a, string) result) (response : query_response) : ('a, string) result =
    match response.rows with
    | [row] ->
      f row
      |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding a single_row: %s" msg)
    | _ -> Error (Printf.sprintf "Expected a single row, but got %d rows" (List.length response.rows))

  let many_rows (f : query_response_row -> ('a, string) result) (response : query_response) : ('a list, string) result =
    response.rows
    |> map_result_l_i (fun i row ->
        f row
        |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding row %d: %s" i msg))

  let single_field (f : string -> ('a, string) result) (row : query_response_row) : ('a, string) result =
    match row.f with
    | [field] -> f field.v
      |> CCResult.map_err (fun msg -> Printf.sprintf "While decoding a single_field: %s" msg)
    | _ -> Error (Printf.sprintf "Expected row with a single field, but got %d fields" (List.length row.f))

  let int str =
    CCInt.of_string str
    |> CCOpt.to_result (Printf.sprintf "Expected an int, but got: %S" str)

  let string (str : string) = Ok str

  let query ?project_id ?(use_legacy_sql=false) ?(params = []) q : (query_response, [> Error.t ]) Lwt_result.t =
    let parameter_mode =
      if use_legacy_sql || params = [] then
        None
      else
        Some NAMED
    in

    let request =
      { kind = "bigquery#queryRequest"
      ; query = q
      ; useLegacySql = use_legacy_sql
      ; location = "EU"
      ; parameterMode = parameter_mode
      ; queryParameters = params
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
        Logs_lwt.debug (fun m -> m "Query: %s" q) |> Lwt_result.ok >>= fun () ->
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
      Logs_lwt.debug (fun m ->
          m "Response: total_bytes_processed=%s cache_hit=%b total_rows=%s"
            response.totalBytesProcessed response.cacheHit response.totalRows
        ) |> Lwt_result.ok >>= fun () ->
      Lwt_result.return response

    | status_code ->
      Error.of_response_status_code_and_body status_code body
end
