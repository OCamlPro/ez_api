open EzAPIServerUtils

(** Looks up request headers and returns map of cookies (cookie_name->cokie_value) *)
val get : Req.t -> string StringMap.t

(** Discards cookie in the 'Set-Cookie' header by setting its value to empty string and
its max-age to 0*)
val clear : name:string -> unit -> (string * string)

(** Creates 'Set-Cokie' header with cookie whose name, value and some parameters are
specified. *)
val set :
  ?secure:bool ->
  ?http_only:bool ->
  ?expiration:int64 ->
  name:string -> value:string -> unit -> (string * string)
