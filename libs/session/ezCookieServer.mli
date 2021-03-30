open EzAPIServerUtils

val get : Req.t -> string StringMap.t
val clear : Req.t -> name:string -> (string * string)
val set :
  ?secure:bool ->
  ?http_only:bool ->
  Req.t -> name:string -> value:string -> (string * string)
