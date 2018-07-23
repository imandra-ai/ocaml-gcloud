
[@@@warning "-39"]
type error_response_errors_item =
  { domain : string
  ; reason : string
  ; message : string
  } [@@deriving yojson]

type t =
  { errors : error_response_errors_item list
  ; code: int
  ; message: string
  } [@@deriving yojson]

type error_response =
  { error : t
  } [@@deriving yojson]

[@@@warning "+39"]
