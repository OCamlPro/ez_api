open EzServerEioUtils

val run : ?catch:(string -> exn -> string Answer.t) ->
  ?allow_origin:Cors.allow_kind -> ?footer:string ->
  env:< net: _ Eio.Net.t; domain_mgr: _ Eio.Domain_manager.t; .. > ->
  sw:Eio.Switch.t -> (int * server_kind) list -> unit
