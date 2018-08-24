module Scopes : sig
  val bigquery : string
end

module Schema : sig
  type mode = REQUIRED | NULLABLE | REPEATED
  val show_mode : mode -> string

  type bq_type =
    | BOOL
    | INTEGER
    | NUMERIC
    | STRING
    | DATE
    | TIME
    | TIMESTAMP
    | RECORD
  val show_bq_type : bq_type -> string

  type field
  val make_field : name:string -> ?description:string option -> mode:mode -> bq_type:bq_type -> ?fields:field list -> unit -> field
  val field_to_yojson : field -> Yojson.Safe.json
end

module Datasets : sig
  val list : unit -> (string, [> Error.t ]) Lwt_result.t
end

module Jobs : sig
  module Param : sig
    type numeric
    type date
    type time
    type timestamp
    type 'a param'

    val bool : bool -> bool param'
    val integer : int -> int param'
    val numeric : string -> numeric param'
    val string : string -> string param'
    val date : string -> date param'
    val time : string -> time param'
    val timestamp : string -> timestamp param'
    val array : ('a -> 'b param') -> 'a list -> 'b param'

    type 'a struct_param
    val struct_ : 'a struct_param -> 'a param'
    val empty_struct : unit struct_param
    val with_field : name:string -> 'a param' -> 'b struct_param -> ('a * 'b) struct_param

    type query_parameter

    val make : name:string -> 'a param' -> query_parameter
  end

  type job_reference =
    { jobId : string
    ; projectId : string
    ; location : string
    }

  type query_response_schema =
    { fields : Schema.field list}

  type query_response_field =
    { v : string }

  type query_response_row =
    { f : query_response_field list}

  type query_response =
    { kind : string
    ; schema : query_response_schema
    ; rows : query_response_row list
    ; pageToken : string option
    ; totalRows : string
    ; jobReference : job_reference
    ; jobComplete : bool
    ; totalBytesProcessed : string
    ; cacheHit : bool
    }

  val query_response_to_yojson : query_response -> Yojson.Safe.json

  val query : ?project_id:string -> ?use_legacy_sql:bool -> ?params:Param.query_parameter list -> string -> (query_response, [> Error.t ]) Lwt_result.t

  val single_row : (query_response_row -> ('a, string) result) -> query_response -> ('a, string) result
  val many_rows : (query_response_row -> ('a, string) result) -> query_response -> ('a list, string) result
  val single_field : (string -> ('a, string) result) -> query_response_row -> ('a, string) result
  val int : string -> (int, string) result
  val string : string -> (string, string) result
end
