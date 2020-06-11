open StringCompat

val get : EzAPI.request -> string StringMap.t
val clear : EzAPI.request -> name:string -> unit
val set :
  ?secure:bool ->
  ?http_only:bool ->
  EzAPI.request -> name:string -> value:string -> unit
