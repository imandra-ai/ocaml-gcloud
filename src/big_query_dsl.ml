module Util = struct
  let pp_comma_sep_list :
        'a.    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list
        -> unit =
   fun pp_item fmt xs ->
    Format.(
      fprintf
        fmt
        "%a"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_item)
        xs)


  let some :
        'a.    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option
        -> unit =
   fun pp_item fmt -> function None -> () | Some x -> pp_item fmt x


  let pp_non_empty_list :
        'a.    (Format.formatter -> 'a list -> unit) -> Format.formatter
        -> 'a list -> unit =
   fun pp_list fmt -> function [] -> () | xs -> pp_list fmt xs


  let pp_string fmt str = Format.fprintf fmt "%s" str

  module Opt = struct
    let map f = function None -> None | Some x -> Some (f x)

    let map_or ~default f = function None -> default | Some x -> f x
  end
end

module rec Expression : sig
  type value

  val sql_of_value : value -> string

  type t

  type type_ =
    | DATE
    | INT64
    | NUMERIC
    | STRING

  type direction

  type window_specification

  type interval

  type date_part =
    | MICROSECOND
    | MILLISECOND
    | SECOND
    | MINUTE
    | HOUR
    | DAY
    | WEEK
    | MONTH
    | QUARTER
    | YEAR
    | DATE
    | TIME

  val pp : Format.formatter -> t -> unit

  val pp_type : Format.formatter -> type_ -> unit

  val pp_window_specification : Format.formatter -> window_specification -> unit

  val pp_order_by : Format.formatter -> t * direction -> unit

  val pp_direction : Format.formatter -> direction -> unit

  val pp_date_part : Format.formatter -> date_part -> unit

  val unsafe : string -> t

  val ident : string -> t

  val field : t -> string -> t

  val param : string -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  val case : ?expr:t -> ?else_:t -> (t * t) list -> t

  val ( && ) : t -> t -> t

  val ( || ) : t -> t -> t

  val ands : t list -> t
  (** Combine [ts] with [&&], or [true] for the empty list *)

  val ands_opt : t list -> t option
  (** Combine [ts] with [&&], or [None] for the empty list. *)

  val ors : t list -> t
  (** Combine [ts] with [||], or [true] for the empty list *)

  val is_null : t -> t

  val is_not_null : t -> t

  val ( = ) : t -> t -> t

  val is_not_distinct_from : t -> t -> t

  val ( <> ) : t -> t -> t

  val ( < ) : t -> t -> t

  val ( <= ) : t -> t -> t

  val ( > ) : t -> t -> t

  val ( >= ) : t -> t -> t

  val not : t -> t

  val in_ : t -> t list -> t

  val in_unnest : t -> t -> t

  val null : t

  val bool : bool -> t

  val int : int -> t

  val zero : t

  val float : float -> t

  val string : string -> t

  val array_lit : t list -> t

  val star : t

  val count : ?distinct:t -> unit -> t

  val cast : as_:type_ -> t -> t

  val over :
       ?named_window:string
    -> ?partition_by:t list
    -> ?order_by:(t * direction) list
    -> ?window_frame_clause:string
    -> t
    -> t

  val window_specification :
       ?named_window:string
    -> ?partition_by:t list
    -> ?order_by:(t * direction) list
    -> ?window_frame_clause:string
    -> unit
    -> window_specification

  val if_ : t -> t -> t -> t

  val countif : t -> t

  val query : Query_expr.t -> t
  (** Embed a SELECT query *)

  val array : Query_expr.t -> t

  val offset : i:t -> t -> t

  val exists : Query_expr.t -> t

  val sum : t -> t

  val avg : t -> t

  val approx_quantiles : t -> int -> t

  val log : t -> t -> t

  val pow : t -> t -> t

  val min : t -> t

  val max : t -> t

  val any_value : t -> t

  val logical_or : t -> t

  val logical_and : t -> t

  val abs : t -> t

  val floor : t -> t

  val ceil : t -> t

  val round : t -> t

  val coalesce : t -> t -> t

  val concat : t list -> t

  val greatest : t list -> t

  val least : t list -> t

  val to_json_string : t -> t

  val row_number : unit -> t

  val generate_array : t -> t -> t -> t

  val generate_date_array : t -> t -> t -> t

  val array_length : t -> t

  val struct_ : t list -> t

  val format : string -> t -> t

  val datetime_of_timestamp : t -> zone:string -> t

  val datetime_of_date_time : t -> t -> t

  val date : t -> t

  val timestamp : t -> t

  val timestamp_add : t -> t -> t

  val timestamp_diff : t -> t -> date_part -> t

  val timestamp_trunc : t -> date_part -> t

  val time_diff : t -> t -> date_part -> t

  val extract : ?at_time_zone:string -> date_part -> from:t -> t

  val interval : t -> date_part -> t

  val asc : direction

  val desc : direction

  val prefix_ident : string -> t -> t

  val is_aggregate : t -> bool
end = struct
  (* TODO: rename to literal? *)
  type value =
    | V_null
    | V_bool of bool
    | V_int of int
    | V_float of float
    | V_string of string

  let sql_of_value = function
    | V_null ->
        "NULL"
    | V_bool b ->
        string_of_bool b
    | V_int i ->
        string_of_int i
    | V_float f ->
        string_of_float f
    | V_string s ->
        Printf.sprintf "'%s'" s


  [@@@warning "-30"]

  type t =
    | Raw of string
    | Star
    | Identifier of string
    | Value of value
    | Array_lit of t list
    | Param of string
    | Field of t * string  (** expr.field *)
    | Case of t option * (t * t) list * t option
    | Plus of t * t
    | Minus of t * t
    | Mul of t * t
    | Div of t * t
    | Not of t
    | And of t list
    | Or of t list
    | Is_null of t * bool
    | Equal of t * t
    | Is_not_distinct_from of t * t
    | Neq of t * t
    | LT of t * t
    | LTE of t * t
    | GT of t * t
    | GTE of t * t
    | In of t * t list
    | In_unnest of t * t
    | Count of t option
        (** Count (Some e) means COUNT(DISTINCT e)
            Count None     means COUNT( * )
        *)
    | Cast of t * type_
    | Func of fn_info
    | Over of t * window_specification
    | Interval of t * date_part
    | Timestamp_diff of t * t * date_part
    | Timestamp_trunc of t * date_part
    | Time_diff of t * t * date_part
    | Extract of date_part * t * string option
    | Query of Query_expr.t
    | Array of Query_expr.t
    | Offset of t * t  (** expr[OFFSET(i)] *)
    | Exists of Query_expr.t

  and fn_info =
    { name : string
    ; args : t list
    ; fn_type : fn_type
    }

  and fn_type =
    | Standard
    | Aggregate
    | Window

  and type_ =
    | DATE
    | INT64
    | NUMERIC
    | STRING

  and direction =
    | Asc
    | Desc

  and window_frame_clause = string

  and window_specification =
    { named_window : string option
    ; partition_by : t list option
    ; order_by : (t * direction) list option
    ; window_frame_clause : window_frame_clause option
    }

  and interval = t * date_part

  and date_part =
    | MICROSECOND
    | MILLISECOND
    | SECOND
    | MINUTE
    | HOUR
    | DAY
    | WEEK
    | MONTH
    | QUARTER
    | YEAR
    | DATE
    | TIME

  [@@@warning "+30"]

  let apply f e =
    match e with
    | Raw _ | Star | Identifier _ | Value _ | Param _ ->
        e
    | Array_lit es ->
        Array_lit (List.map f es)
    | Field (e, field) ->
        Field (f e, field)
    | Case (e, whens, else_) ->
        Case
          ( Util.Opt.map f e
          , List.map (fun (e1, e2) -> (f e1, f e2)) whens
          , match else_ with None -> None | Some e -> Some (f e) )
    | Plus (e1, e2) ->
        Plus (f e1, f e2)
    | Minus (e1, e2) ->
        Minus (f e1, f e2)
    | Mul (e1, e2) ->
        Mul (f e1, f e2)
    | Div (e1, e2) ->
        Div (f e1, f e2)
    | Not e ->
        Not (f e)
    | Is_null (e, b) ->
        Is_null (f e, b)
    | Equal (e1, e2) ->
        Equal (f e1, f e2)
    | Is_not_distinct_from (e1, e2) ->
        Is_not_distinct_from (f e1, f e2)
    | Neq (e1, e2) ->
        Neq (f e1, f e2)
    | LT (e1, e2) ->
        LT (f e1, f e2)
    | LTE (e1, e2) ->
        LTE (f e1, f e2)
    | GT (e1, e2) ->
        GT (f e1, f e2)
    | GTE (e1, e2) ->
        GTE (f e1, f e2)
    | In (e1, e2) ->
        In (f e1, List.map f e2)
    | In_unnest (e1, e2) ->
        In_unnest (f e1, f e2)
    | And es ->
        And (List.map f es)
    | Or es ->
        Or (List.map f es)
    | Count e ->
        Count (match e with None -> None | Some e -> Some (f e))
    | Cast (e1, t) ->
        Cast (f e1, t)
    | Func v ->
        Func { v with args = List.map f v.args }
    | Over (e, window_specification) ->
        Over (f e, window_specification)
    | Interval (e, date_part) ->
        Interval (f e, date_part)
    | Timestamp_diff (e1, e2, date_part) ->
        Timestamp_diff (f e1, f e2, date_part)
    | Timestamp_trunc (e, date_part) ->
        Timestamp_trunc (f e, date_part)
    | Time_diff (e1, e2, date_part) ->
        Time_diff (f e1, f e2, date_part)
    | Extract (date_part, e, at_time_zone) ->
        Extract (date_part, f e, at_time_zone)
    | Query q ->
        Query q
    | Array q ->
        Array q
    | Offset (e, i) ->
        Offset (f e, i)
    | Exists q ->
        Exists q


  let rec pp fmt =
    let open Format in
    function
    | Raw sql ->
        Format.fprintf fmt "%s" sql
    | Star ->
        Format.fprintf fmt "*"
    | Identifier i ->
        Format.fprintf fmt "%s" i
    | Value v ->
        Format.fprintf fmt "%s" (sql_of_value v)
    | Param param ->
        Format.fprintf fmt "@%s" param
    | Array_lit es ->
        fprintf
          fmt
          "@[<hv 1>[ %a@ ]@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp)
          es
    | Field (e, field) ->
        Format.fprintf fmt "%a.%s" pp_parens e field
    | Case (e, whens, else_) ->
        Format.fprintf
          fmt
          "CASE %a %a%a END"
          (Util.some pp)
          e
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
             (fun fmt (when_, then_) ->
               Format.fprintf fmt "WHEN %a THEN %a" pp when_ pp then_ ) )
          whens
          (fun fmt -> function
            | Some else_ ->
                Format.fprintf fmt " ELSE %a" pp else_
            | None ->
                () )
          else_
    | Plus (e1, e2) ->
        Format.fprintf fmt "@[<hv 1>(%a@ + %a)@]" pp e1 pp e2
    | Minus (e1, e2) ->
        Format.fprintf fmt "@[<hv 1>(%a@ - %a)@]" pp e1 pp e2
    | Mul (e1, e2) ->
        Format.fprintf fmt "@[<hv 1>(%a@ * %a)@]" pp e1 pp e2
    | Div (e1, e2) ->
        Format.fprintf fmt "@[<hv 1>(%a@ / %a)@]" pp e1 pp e2
    | Not e ->
        Format.fprintf fmt "NOT %a" pp e
    | Is_null (e, b) ->
        Format.fprintf fmt "%a IS%s NULL" pp e (if b then "" else " NOT")
    | Equal (e1, e2) ->
        Format.fprintf fmt "%a = %a" pp_parens e1 pp_parens e2
    | Is_not_distinct_from (e1, e2) ->
        Format.fprintf fmt "%a IS NOT DISTINCT FROM %a" pp e1 pp e2
    | Neq (e1, e2) ->
        Format.fprintf fmt "%a <> %a" pp e1 pp e2
    | LT (e1, e2) ->
        Format.fprintf fmt "%a < %a" pp e1 pp e2
    | LTE (e1, e2) ->
        Format.fprintf fmt "%a <= %a" pp e1 pp e2
    | GT (e1, e2) ->
        Format.fprintf fmt "%a > %a" pp e1 pp e2
    | GTE (e1, e2) ->
        Format.fprintf fmt "%a >= %a" pp e1 pp e2
    | In (e1, e2) ->
        Format.fprintf
          fmt
          "%a IN (%a)"
          pp
          e1
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp )
          e2
    | In_unnest (e1, e2) ->
        Format.fprintf fmt "%a IN UNNEST(%a)" pp e1 pp e2
    | And es ->
        Format.(
          fprintf
            fmt
            "@[<hv 1>(%a)@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ AND ") pp)
            es)
    | Or es ->
        Format.(
          fprintf
            fmt
            "@[<hv 1>(%a)@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ OR ") pp)
            es)
    | Count e ->
        Format.(
          fprintf
            fmt
            "COUNT(%a)"
            (fun fmt e ->
              match e with
              | Some e ->
                  fprintf fmt "DISTINCT %a" pp e
              | None ->
                  fprintf fmt "*" )
            e)
    | Cast (e1, t) ->
        Format.fprintf fmt "CAST(%a AS %a)" pp e1 pp_type t
    | Func { name; args; _ } ->
        Format.fprintf
          fmt
          "@[<hv 2>%s(@,%a)@]"
          name
          (Util.pp_comma_sep_list pp)
          args
    | Over (e, window_specification) ->
        Format.fprintf
          fmt
          "@[<hv 2>%a OVER (@,%a)@]"
          pp
          e
          pp_window_specification
          window_specification
    | Interval (e, date_part) ->
        Format.fprintf fmt "INTERVAL %a %a" pp e pp_date_part date_part
    | Timestamp_diff (e1, e2, date_part) ->
        Format.fprintf
          fmt
          "@[<hv 2>TIMESTAMP_DIFF(@,%a,@ %a,@ %a)@]"
          pp
          e1
          pp
          e2
          pp_date_part
          date_part
    | Timestamp_trunc (e, date_part) ->
        Format.fprintf
          fmt
          "@[<hv 2>TIMESTAMP_TRUNC(@,%a,@ %a)@]"
          pp
          e
          pp_date_part
          date_part
    | Time_diff (e1, e2, date_part) ->
        Format.fprintf
          fmt
          "@[<hv 2>TIME_DIFF(@,%a,@ %a,@ %a)@]"
          pp
          e1
          pp
          e2
          pp_date_part
          date_part
    | Extract (date_part, e, at_time_zone) ->
        Format.fprintf
          fmt
          "EXTRACT(%a FROM %a%a)"
          pp_date_part
          date_part
          pp
          e
          (Util.some (fun fmt tz -> Format.fprintf fmt " AT TIME ZONE %S" tz))
          at_time_zone
    | Query q ->
        Format.fprintf fmt "@[<hv 1>(%a)@]" Query_expr.pp q
    | Array q ->
        Format.fprintf fmt "@[<hv 2>ARRAY(@,%a@])" Query_expr.pp q
    | Offset (e, i) ->
        Format.fprintf fmt "%a[OFFSET(%a)]" pp_parens e pp i
    | Exists q ->
        Format.fprintf fmt "@[<hv 2>EXISTS (@,%a@])" Query_expr.pp q


  and pp_parens fmt t =
    let open Format in
    match t with
    | Identifier _ | Star | Field _ | Param _ ->
        pp fmt t
    | _ ->
        fprintf fmt "(%a)" pp t


  and pp_type fmt t =
    Format.(
      fprintf
        fmt
        "%s"
        ( match t with
        | DATE ->
            "DATE"
        | INT64 ->
            "INT64"
        | NUMERIC ->
            "NUMERIC"
        | STRING ->
            "STRING" ))


  and pp_window_specification fmt w =
    Format.(
      fprintf
        fmt
        "%a%a%a%a"
        Util.(some (fun fmt s -> fprintf fmt "@[%s@]" s))
        w.named_window
        Util.(
          some (fun fmt es ->
              fprintf fmt "@[<hv 2>PARTITION BY@ %a@]" (pp_comma_sep_list pp) es ))
        w.partition_by
        Util.(
          some (fun fmt order_bys ->
              fprintf
                fmt
                "@ @[<hv 2>ORDER BY@ %a@]"
                (pp_comma_sep_list pp_order_by)
                order_bys ))
        w.order_by
        Util.(
          some (fun fmt window_frame_clause ->
              fprintf fmt "@ @[<hv 2>%s@]" window_frame_clause ))
        w.window_frame_clause)


  and pp_order_by fmt (expression, direction) =
    Format.(fprintf fmt "%a %a" pp expression pp_direction direction)


  and pp_direction fmt =
    Format.(function Asc -> fprintf fmt "ASC" | Desc -> fprintf fmt "DESC")


  and pp_date_part fmt date_part =
    Format.(
      fprintf
        fmt
        ( match date_part with
        | MICROSECOND ->
            "MICROSECOND"
        | MILLISECOND ->
            "MILLISECOND"
        | SECOND ->
            "SECOND"
        | MINUTE ->
            "MINUTE"
        | HOUR ->
            "HOUR"
        | DAY ->
            "DAY"
        | WEEK ->
            "WEEK"
        | MONTH ->
            "MONTH"
        | QUARTER ->
            "QUARTER"
        | YEAR ->
            "YEAR"
        | DATE ->
            "DATE"
        | TIME ->
            "TIME" ))


  let rec prefix_ident prefix = function
    | Identifier i ->
        Identifier (prefix ^ "." ^ i)
    | e ->
        apply (prefix_ident prefix) e


  (** Returns true if any of the functions called in the expression is an aggreate function. *)
  let rec is_aggregate = function
    | Raw _ | Star | Identifier _ | Value _ | Param _ ->
        false
    | Array_lit es ->
        List.exists is_aggregate es
    | Field (e, _) ->
        is_aggregate e
    | Case (e, whens, else_) ->
        Util.Opt.map_or ~default:false is_aggregate e
        || List.exists
             (fun (e1, e2) -> is_aggregate e1 || is_aggregate e2)
             whens
        || (match else_ with None -> false | Some e -> is_aggregate e)
    | Plus (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Minus (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Mul (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Div (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Not e ->
        is_aggregate e
    | Is_null (e, _) ->
        is_aggregate e
    | Equal (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Is_not_distinct_from (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | Neq (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | LT (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | LTE (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | GT (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | GTE (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | In (e1, e2) ->
        is_aggregate e1 || List.exists is_aggregate e2
    | In_unnest (e1, e2) ->
        is_aggregate e1 || is_aggregate e2
    | And es ->
        List.exists is_aggregate es
    | Or es ->
        List.exists is_aggregate es
    | Count _ ->
        true
    | Cast (e1, _) ->
        is_aggregate e1
    | Func { fn_type = Aggregate; _ } ->
        true
    | Func v ->
        List.exists is_aggregate v.args
    | Over (e, _window_specification) ->
        is_aggregate e
    | Interval (e, _date_part) ->
        is_aggregate e
    | Timestamp_diff (e1, e2, _) ->
        is_aggregate e1 || is_aggregate e2
    | Timestamp_trunc (e, _) ->
        is_aggregate e
    | Time_diff (e1, e2, _) ->
        is_aggregate e1 || is_aggregate e2
    | Extract (_, e, _) ->
        is_aggregate e
    | Query _ ->
        false
    | Array _ ->
        false
    | Offset (e, _) ->
        is_aggregate e
    | Exists _ ->
        false


  let unsafe s = Raw s

  let ident i = Identifier i

  let field e f = Field (e, f)

  let param s = Param s

  let ( + ) e1 e2 = Plus (e1, e2)

  let ( - ) e1 e2 = Minus (e1, e2)

  let ( * ) e1 e2 = Mul (e1, e2)

  let ( / ) e1 e2 = Div (e1, e2)

  let case ?expr ?else_ whens = Case (expr, whens, else_)

  let bool b = Value (V_bool b)

  let ( && ) e1 e2 =
    match (e1, e2) with
    | And e1s, And e2s ->
        And (e1s @ e2s)
    | And e1s, e2 ->
        And (e1s @ [ e2 ])
    | e1, And e2s ->
        And (e1 :: e2s)
    | e1, e2 ->
        And [ e1; e2 ]


  let ands = function e :: es -> List.fold_left ( && ) e es | [] -> bool true

  let ands_opt = function
    | e :: es ->
        Some (List.fold_left ( && ) e es)
    | [] ->
        None


  let ( || ) e1 e2 =
    match (e1, e2) with
    | Or e1s, Or e2s ->
        Or (e1s @ e2s)
    | Or e1s, e2 ->
        Or (e1s @ [ e2 ])
    | e1, Or e2s ->
        Or (e1 :: e2s)
    | e1, e2 ->
        Or [ e1; e2 ]


  let ors = function e :: es -> List.fold_left ( || ) e es | [] -> bool true

  let is_null e = Is_null (e, true)

  let is_not_null e = Is_null (e, false)

  let ( = ) e1 e2 = Equal (e1, e2)

  let is_not_distinct_from e1 e2 = Is_not_distinct_from (e1, e2)

  let ( <> ) e1 e2 = Neq (e1, e2)

  let ( < ) e1 e2 = LT (e1, e2)

  let ( <= ) e1 e2 = LTE (e1, e2)

  let ( > ) e1 e2 = GT (e1, e2)

  let ( >= ) e1 e2 = GTE (e1, e2)

  let not e = Not e

  let in_ e1 e2 = In (e1, e2)

  let in_unnest e1 e2 = In_unnest (e1, e2)

  let null = Value V_null

  let int x = Value (V_int x)

  let zero = int 0

  let float x = Value (V_float x)

  let string x = Value (V_string x)

  let array_lit es = Array_lit es

  let star = Star

  let count ?distinct () = Count distinct

  let cast ~as_:t e = Cast (e, t)

  let window_specification
      ?named_window ?partition_by ?order_by ?window_frame_clause () =
    { named_window; partition_by; order_by; window_frame_clause }


  let over ?named_window ?partition_by ?order_by ?window_frame_clause e =
    Over (e, { named_window; partition_by; order_by; window_frame_clause })


  let make_fn ?(fn_type = Standard) name args = Func { name; args; fn_type }

  let if_ cond then_ else_ = make_fn "IF" [ cond; then_; else_ ]

  let countif e = make_fn "COUNTIF" [ e ] ~fn_type:Aggregate

  let sum e = make_fn "SUM" [ e ] ~fn_type:Aggregate

  let avg e = make_fn "AVG" [ e ] ~fn_type:Aggregate

  let approx_quantiles e n =
    make_fn "APPROX_QUANTILES" [ e; int n ] ~fn_type:Aggregate


  let log e1 e2 = make_fn "LOG" [ e1; e2 ]

  let pow e1 e2 = make_fn "POW" [ e1; e2 ]

  let min e = make_fn "MIN" [ e ] ~fn_type:Aggregate

  let max e = make_fn "MAX" [ e ] ~fn_type:Aggregate

  let any_value e = make_fn "ANY_VALUE" [ e ] ~fn_type:Aggregate

  let logical_or e = make_fn "LOGICAL_OR" [ e ] ~fn_type:Aggregate

  let logical_and e = make_fn "LOGICAL_AND" [ e ] ~fn_type:Aggregate

  let abs e = make_fn "ABS" [ e ]

  let floor e = make_fn "FLOOR" [ e ]

  let ceil e = make_fn "CEIL" [ e ]

  let round e = make_fn "ROUND" [ e ]

  let coalesce e1 e2 = make_fn "COALESCE" [ e1; e2 ]

  let concat es = make_fn "CONCAT" es

  let greatest es = make_fn "GREATEST" es

  let least es = make_fn "LEAST" es

  let to_json_string e = make_fn "TO_JSON_STRING" [ e ]

  let row_number () = make_fn "ROW_NUMBER" [] ~fn_type:Window

  let generate_array min max step = make_fn "GENERATE_ARRAY" [ min; max; step ]

  let generate_date_array min max step =
    make_fn "GENERATE_DATE_ARRAY" [ min; max; step ]


  let array_length e = make_fn "ARRAY_LENGTH" [ e ]

  let struct_ es =
    (* TODO STRUCT( expr1 [AS field_name] [, ... ]) *)
    make_fn "STRUCT" es


  let format str e = make_fn "FORMAT" [ string str; e ]

  let datetime_of_timestamp timestamp_e ~zone =
    make_fn "DATETIME" [ timestamp_e; string zone ]


  let datetime_of_date_time date time = make_fn "DATETIME" [ date; time ]

  let date timestamp_e = make_fn "DATE" [ timestamp_e ]

  let timestamp t = make_fn "TIMESTAMP" [ t ]

  let timestamp_add t interval = make_fn "TIMESTAMP_ADD" [ t; interval ]

  let timestamp_diff t1 t2 date_part = Timestamp_diff (t1, t2, date_part)

  let timestamp_trunc t date_part = Timestamp_trunc (t, date_part)

  let time_diff t1 t2 date_part = Time_diff (t1, t2, date_part)

  let extract ?at_time_zone date_part ~from =
    Extract (date_part, from, at_time_zone)


  let interval e date_part = Interval (e, date_part)

  let asc = Asc

  let desc = Desc

  let query q = Query q

  let array q = Array q

  let offset ~i array = Offset (array, i)

  let exists q = Exists q
end

and Query_expr : sig
  type t

  type limit

  type select

  type select_body

  type select_item

  type from_item

  type join

  type join_type =
    | INNER
    | CROSS
    | FULL
    | LEFT
    | RIGHT

  type named_window

  val with_aliased : as_:string -> from_item -> from_item

  val pp : Format.formatter -> t -> unit

  val pp_with : Format.formatter -> string * t -> unit

  val pp_limit : Format.formatter -> limit -> unit

  val pp_offset : Format.formatter -> Expression.t -> unit

  val pp_select : Format.formatter -> select -> unit

  val pp_from_item : Format.formatter -> from_item -> unit

  val pp_join : Format.formatter -> join -> unit

  val pp_join_type : Format.formatter -> join_type -> unit

  val pp_alias : Format.formatter -> string -> unit

  val to_string : t -> string

  val e : ?except:string list -> ?as_:string -> Expression.t -> select_item

  val ident : ?as_:string -> string -> from_item

  val query_expr : ?as_:string -> t -> from_item

  val join :
       ?on:Expression.t
    -> ?using:Expression.t list
    -> ?type_:join_type
    -> from_item
    -> from_item
    -> from_item

  val inner_join :
       ?on:Expression.t
    -> ?using:Expression.t list
    -> from_item
    -> from_item
    -> from_item

  val left_join :
       ?on:Expression.t
    -> ?using:Expression.t list
    -> from_item
    -> from_item
    -> from_item

  val cross_join : from_item -> from_item -> from_item

  val unnest :
    ?as_:string -> ?with_offset_as:string -> Expression.t -> from_item

  val named_window :
       ?named_window:string
    -> ?partition_by:Expression.t list
    -> ?order_by:(Expression.t * Expression.direction) list
    -> ?window_frame_clause:string
    -> string
    -> named_window

  val select :
       ?distinct:bool
    -> ?as_struct:bool
    -> ?from:from_item
    -> ?where:Expression.t
    -> ?group_by:Expression.t list
    -> ?having:Expression.t
    -> ?window:named_window list
    -> select_item list
    -> select

  val with_ : 'a -> as_:'b -> 'a * 'b

  val limit : ?offset:Expression.t -> Expression.t -> limit

  val make :
       ?with_:(string * t) list
    -> ?order_by:(Expression.t * Expression.direction) list
    -> ?limit:limit
    -> select
    -> t

  val expression_of_select_item : select_item -> Expression.t

  val alias_of_select_item : select_item -> string option

  val union_all : select -> select -> select
end = struct
  type t =
    { with_ : (string * t) list
    ; select : select
    ; order_by : (Expression.t * Expression.direction) list
    ; limit : limit option
    }

  and limit =
    { (* Note: these expressions must be either literal ints or parameters *)
      count : Expression.t
    ; offset : Expression.t option
    }

  and select =
    | Select of select_body
    | Union_all of select_body list

  and select_body =
    { distinct : bool
    ; as_struct : bool
    ; expressions : select_item list
    ; from : from_item option
    ; where : Expression.t option
    ; group_by : Expression.t list
    ; having : Expression.t option
    ; window : named_window list
    }

  and select_item =
    { expression : Expression.t
    ; alias : string option
    ; except : string list
    }

  and from_item =
    | From_ident of string * string option
    | From_query_expr of t * string option
    | From_join of join
    | From_unnest of Expression.t * string option (* as *) * string option

  (* with offset as *)
  and join =
    { left : from_item
    ; right : from_item
    ; on : Expression.t option
    ; using : Expression.t list
    ; type_ : join_type option
    }

  and join_type =
    | INNER
    | CROSS
    | FULL
    | LEFT
    | RIGHT

  and named_window =
    { name : string
    ; window_specification : Expression.window_specification
    }

  let with_aliased ~as_ from_item =
    match from_item with
    | From_ident (table_name, _) ->
        From_ident (table_name, Some as_)
    | From_query_expr (t, _) ->
        From_query_expr (t, Some as_)
    | From_join join ->
        From_join join
    | From_unnest (expression, _, with_offset_as) ->
        From_unnest (expression, Some as_, with_offset_as)


  let rec pp fmt t =
    Format.(
      fprintf
        fmt
        "%a%a%a%a"
        (Util.pp_non_empty_list (fun fmt withs ->
             fprintf
               fmt
               "@[<2>WITH@ %a@]@ "
               (Util.pp_comma_sep_list pp_with)
               withs ) )
        t.with_
        pp_select
        t.select
        (Util.pp_non_empty_list (fun fmt order_bys ->
             fprintf
               fmt
               "@ @[<hv 2>ORDER BY@ %a@]"
               (Util.pp_comma_sep_list Expression.pp_order_by)
               order_bys ) )
        t.order_by
        Util.(some pp_limit)
        t.limit)


  and pp_with fmt (with_query_name, query_expr) =
    Format.(fprintf fmt "@[<hv 2>%s AS (@,%a)@]" with_query_name pp query_expr)


  and pp_limit fmt limit =
    Format.(
      fprintf
        fmt
        "@ LIMIT %a%a"
        Expression.pp
        limit.count
        (Util.some pp_offset)
        limit.offset)


  and pp_offset fmt offset =
    Format.(fprintf fmt "@ OFFSET %a" Expression.pp offset)


  and pp_named_window fmt w =
    Format.(
      fprintf
        fmt
        "@[<hv 2>%s AS (@,%a)@]"
        w.name
        Expression.pp_window_specification
        w.window_specification)


  and pp_select_body fmt (select_body : select_body) =
    Format.(
      let pp_group_by fmt expressions =
        fprintf
          fmt
          "@ @[<hv 2>GROUP BY@ %a@]"
          (Util.pp_comma_sep_list Expression.pp)
          expressions
      in
      let pp_window fmt expressions =
        fprintf
          fmt
          "@ @[<hv 2>WINDOW@ %a@]"
          (Util.pp_comma_sep_list pp_named_window)
          expressions
      in
      fprintf
        fmt
        "@[<hv>@[<hv 2>SELECT@ %a%a%a@]%a%a%a%a%a@]"
        (fun fmt distinct -> if distinct then fprintf fmt "DISTINCT " else ())
        select_body.distinct
        (fun fmt as_struct -> if as_struct then fprintf fmt "AS STRUCT " else ())
        select_body.as_struct
        (Util.pp_comma_sep_list pp_select_expression)
        select_body.expressions
        (Util.some (fun fmt from_item ->
             fprintf fmt "@ @[<hv 2>FROM@ %a@]" pp_from_item from_item ) )
        select_body.from
        (Util.some (fun fmt where ->
             fprintf fmt "@ WHERE %a" Expression.pp where ) )
        select_body.where
        (Util.pp_non_empty_list pp_group_by)
        select_body.group_by
        (Util.some (fun fmt having ->
             fprintf fmt "@ HAVING %a" Expression.pp having ) )
        select_body.having
        (Util.pp_non_empty_list pp_window)
        select_body.window)


  and pp_select fmt select =
    match select with
    | Select r ->
        Format.(fprintf fmt "%a" pp_select_body r)
    | Union_all ts ->
        Format.(
          fprintf
            fmt
            "%a"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ @[UNION ALL@]@ ")
               (fun fmt a -> Format.fprintf fmt "@[%a@]" pp_select_body a) )
            ts)


  and pp_select_expression fmt { expression; alias; except } =
    Format.(
      let pp_except fmt names =
        fprintf
          fmt
          "@ @[<hv 2>EXCEPT (@,%a)@]"
          (Util.pp_comma_sep_list Util.pp_string)
          names
      in
      fprintf
        fmt
        "%a%a%a"
        Expression.pp
        expression
        (Util.pp_non_empty_list pp_except)
        except
        (Util.some pp_alias)
        alias)


  and pp_from_item fmt =
    Format.(
      function
      | From_ident (name, alias) ->
          fprintf fmt "%s%a" name (Util.some pp_alias) alias
      | From_query_expr (t, alias) ->
          fprintf fmt "(%a)%a" pp t (Util.some pp_alias) alias
      | From_join join ->
          pp_join fmt join
      | From_unnest (expression, alias, with_offset_as) ->
          fprintf
            fmt
            "@[<hv 2>UNNEST(@,%a)%a%a@]"
            Expression.pp
            expression
            (Util.some pp_alias)
            alias
            (Util.some (fun fmt as_ -> fprintf fmt " WITH OFFSET AS %s" as_))
            with_offset_as)


  and pp_join fmt join =
    Format.(
      fprintf
        fmt
        "@[<hv>%a@ %aJOIN@ %a%a%a@]"
        pp_from_item
        join.left
        (Util.some pp_join_type)
        join.type_
        pp_from_item
        join.right
        (Util.some (fun fmt on -> fprintf fmt "@ ON %a" Expression.pp on))
        join.on
        (Util.pp_non_empty_list (fun fmt es ->
             fprintf
               fmt
               "@ @[<hv 2>USING (%a)@]"
               (Util.pp_comma_sep_list Expression.pp)
               es ) )
        join.using)


  and pp_join_type fmt type_ =
    let str =
      match type_ with
      | INNER ->
          "INNER"
      | CROSS ->
          "CROSS"
      | FULL ->
          "FULL"
      | LEFT ->
          "LEFT"
      | RIGHT ->
          "RIGHT"
    in
    Format.(fprintf fmt "%s " str)


  and pp_alias fmt alias = Format.(fprintf fmt " AS %s" alias)

  let to_string t = Format.asprintf "%a" pp t

  let e ?(except = []) ?as_ expression = { expression; alias = as_; except }

  let ident ?as_ name = From_ident (name, as_)

  let query_expr ?as_ t = From_query_expr (t, as_)

  let join ?on ?(using = []) ?type_ right left =
    From_join { type_; left; right; on; using }


  let inner_join ?on ?using right left = join ~type_:INNER ?on ?using right left

  let left_join ?on ?using right left = join ~type_:LEFT ?on ?using right left

  let cross_join right left = join ~type_:CROSS right left

  let unnest ?as_ ?with_offset_as e = From_unnest (e, as_, with_offset_as)

  let named_window
      ?named_window ?partition_by ?order_by ?window_frame_clause name =
    { name
    ; window_specification =
        Expression.window_specification
          ?named_window
          ?partition_by
          ?order_by
          ?window_frame_clause
          ()
    }


  let select
      ?(distinct = false)
      ?(as_struct = false)
      ?from
      ?where
      ?(group_by = [])
      ?having
      ?(window = [])
      expressions =
    Select
      { distinct
      ; as_struct
      ; expressions
      ; from
      ; where
      ; group_by
      ; having
      ; window
      }


  let with_ alias ~as_ = (alias, as_)

  let limit ?offset count = { count; offset }

  let make ?(with_ = []) ?(order_by = []) ?limit select =
    { with_; select; order_by; limit }


  let expression_of_select_item ({ expression; _ } : select_item) : Expression.t
      =
    expression


  let alias_of_select_item ({ alias; _ } : select_item) : string option = alias

  let union_all s2 s1 =
    let to_list = function Select s -> [ s ] | Union_all ss -> ss in
    Union_all (to_list s1 @ to_list s2)
end
