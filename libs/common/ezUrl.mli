
val encode : string -> string
val encode_args : (string * string list) list -> string

val decode : string -> string
val decode_args : string -> (string * string list) list

val content_type : string
