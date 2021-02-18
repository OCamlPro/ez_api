
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
  internal error (-1 connection, -2 decoding) *)

val request_reply_hook : (unit -> unit) ref

val log : (string -> unit) ref


(* Engine independent implementation. Beware: if you use these calls,
you must initialize an engine independantly.*)
module ANY : EzReq_S.S

module Make(_ : EzReq_S.Interface) : EzReq_S.S
