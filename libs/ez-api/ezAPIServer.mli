type 'a directory
type 'output answer

val empty : 'a directory
val return : 'a -> 'a answer Lwt.t


exception EzRawReturn of string

val register :
           ('a, 'b, 'input, 'output) EzAPI.service ->
           ('a -> 'input -> 'output answer Lwt.t) ->
           EzAPI.request directory ->
           EzAPI.request directory




type server_kind =
  | API of EzAPI.request directory
  | Root of string * string option

val server : (int * server_kind) list -> unit Lwt.t




val t0 : float
val req_time : unit -> float
val req_ips : (string, EzAPI.ip_info) Hashtbl.t

type timings = {
    mutable timings_ok : Timings.t array;
    mutable timings_fail : Timings.t array;
  }

val timings : timings

val return_error : int -> 'a

val verbose : int
