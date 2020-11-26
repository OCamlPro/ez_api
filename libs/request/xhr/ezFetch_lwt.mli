module Interface : EzRequest_lwt.Interface
include EzRequest_lwt.S

(* hook executed before every xhr *)
val add_hook : (unit -> unit) -> unit
