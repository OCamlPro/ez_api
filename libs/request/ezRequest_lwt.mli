type 'a api_error = 'a EzReq_lwt_S.api_error
type ('output, 'error) api_result = ('output, 'error) EzReq_lwt_S.api_result

val handle_error : ('a -> string option) -> 'a api_error -> int * string option
val string_of_error : ('a -> string option) -> 'a api_error -> string

val request_reply_hook : (unit -> unit) ref

val log : (string -> unit) ref

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)
module ANY : EzReq_lwt_S.S

module Make(_ : EzReq_lwt_S.Interface) : EzReq_lwt_S.S
