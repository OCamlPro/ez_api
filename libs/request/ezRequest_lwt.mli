type 'a api_error = 'a EzReq_lwt_S.api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknownError of { code : int ; msg : string option }

type ('output, 'error) api_result = ('output, 'error) EzReq_lwt_S.api_result

val handle_error : ('a -> string option) -> 'a api_error -> int * string option
val string_of_error : ('a -> string option) -> 'a api_error -> string

val request_reply_hook : (unit -> unit) ref

module type S = EzReq_lwt_S.S
module type Interface = EzReq_lwt_S.Interface

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)
module ANY : S

module Make(_ : Interface) : S
