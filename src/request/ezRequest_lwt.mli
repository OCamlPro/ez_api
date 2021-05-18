val request_reply_hook : (unit -> unit) ref

module type S = EzReq_lwt_S.S
module type Interface = EzReq_lwt_S.Interface

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)
module ANY : S

module Make(_ : Interface) : S
