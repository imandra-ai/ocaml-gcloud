module Scopes : sig
  val bigquery : string
end

module Schema : sig
  type mode =
    | REQUIRED
    | NULLABLE
    | REPEATED

  val show_mode : mode -> string

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

  val show_bq_type : bq_type -> string

  type field

  val make_field :
       name:string
    -> ?description:string option
    -> mode:mode
    -> bq_type:bq_type
    -> ?fields:field list
    -> unit
    -> field

  val field_to_yojson : field -> Yojson.Safe.t

  val bq_type_of_field : field -> bq_type

  val name_of_field : field -> string

  val description_of_field : field -> string option

  val mode_of_field : field -> mode

  val fields_of_field : field -> field list
end

module Datasets : sig
  val get :
       ?project_id:string
    -> dataset_id:string
    -> unit
    -> (string, [> Error.t ]) Lwt_result.t

  val list : ?project_id:string -> unit -> (string, [> Error.t ]) Lwt_result.t

  module Tables : sig
    type table =
      { id : string
      ; kind : string
      ; friendlyName : string option
      ; expirationTime : string option
      ; creationTime : string
      }

    type resp =
      { tables : table list
      ; kind : string
      ; etag : string
      ; nextPageToken : string option
      ; totalItems : int
      }

    val list :
         ?project_id:string
      -> ?max_results:int
      -> ?page_token:string
      -> dataset_id:string
      -> unit
      -> (resp, [> Error.t ]) Lwt_result.t
  end
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

    val array :
      ?default_type:Schema.bq_type -> ('a -> 'b param') -> 'a list -> 'b param'

    type 'a struct_param

    val struct_ : 'a struct_param -> 'a param'

    val empty_struct : unit struct_param

    val with_field :
      name:string -> 'a param' -> 'b struct_param -> ('a * 'b) struct_param

    type query_parameter

    val make : name:string -> 'a param' -> query_parameter

    val name : query_parameter -> string

    val param_to_expression :
      query_parameter -> Gcloud_shared.Big_query_dsl.Expression.t

    module Debug : sig
      val to_string : query_parameter -> string * string
    end
  end

  type job_reference =
    { job_id : string option
    ; project_id : string
    ; location : string
    }

  type query_response_schema = { fields : Schema.field list }

  type value =
    | Null
    | String of string
    | Number of float
    | Bool of bool
    | List of value list
    | Struct of value list

  type query_response_row = { f : value list }

  type query_response_data =
    { schema : query_response_schema
    ; rows : query_response_row list
    ; page_token : string option
    ; total_rows : string option
    ; num_dml_affected_rows : string option
    ; total_bytes_processed : string
    ; cache_hit : bool
    }

  val query_response_row_of_yojson :
    Yojson.Safe.t -> (query_response_row, string) result

  val query_response_row_to_yojson : query_response_row -> Yojson.Safe.t
  val pp_query_response_data : Format.formatter -> query_response_data -> unit

  (** Type of query responses. We may or may not have the data, depending on
      whether the job completed within the timeout.

      Tip: use {!poll_until_complete} to poll the job until we have some concrete data.
  *)
  type query_response =
    { kind : string
    ; job_reference : job_reference
    ; job_complete : query_response_data option
    }

  val query_response_to_yojson : query_response -> Yojson.Safe.t

  val query :
       ?project_id:string
    -> ?dry_run:bool
    -> ?use_legacy_sql:bool
    -> ?params:Param.query_parameter list
    -> ?location:string
    -> ?use_int64_timestamp:bool
    -> string
    -> (query_response, [> Error.t ]) Lwt_result.t

  val get_query_results :
       ?page_token:string
    -> ?use_int64_timestamp:bool
    -> job_reference
    -> (query_response, [> Error.t ]) Lwt_result.t

  (** Type of query responses, when the job is complete and we definitely have some data. *)
  type query_response_complete =
    { kind : string
    ; job_reference : job_reference
    ; data : query_response_data
    }

  val query_response_complete_to_yojson :
    query_response_complete -> Yojson.Safe.t

  val poll_until_complete :
       ?poll_every_s:float
    -> ?attempts:int
    -> query_response
    -> (query_response_complete, [> Error.t ]) result Lwt.t

  val fetch_all_rows :
       query_response_complete
    -> (query_response_complete, [> Error.t ]) Lwt_result.t

  type ('a, 'b) decoder = 'a -> ('b, string) result

  type 'a data_decoder = (query_response_data, 'a) decoder

  type 'a row_decoder = (query_response_row, 'a) decoder

  type 'a value_decoder = (value, 'a) decoder

  val tag : string -> ('a, string) result -> ('a, string) result

  val single_row : 'a row_decoder -> 'a data_decoder

  val many_rows : 'a row_decoder -> 'a list data_decoder

  val single_field : 'a value_decoder -> 'a row_decoder

  val string : string value_decoder

  val int : int value_decoder

  val bool : bool value_decoder

  val float : float value_decoder

  val nullable : 'a value_decoder -> 'a option value_decoder

  val list : 'a value_decoder -> 'a list value_decoder
end
