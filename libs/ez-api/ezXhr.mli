
include EzRequest.S

val init : unit -> unit

(* hook executed before every xhr *)
val add_hook : (unit -> unit) -> unit
