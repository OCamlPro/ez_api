
include EzRequest.S

(* hook executed before every xhr *)
val add_hook : (unit -> unit) -> unit
