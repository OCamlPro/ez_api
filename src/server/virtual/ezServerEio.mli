open EzServerEioUtils

val run : ?catch:(string -> exn -> string Answer.t) ->
  ?allow_origin:Cors.allow_kind -> ?footer:string -> ?addr:string ->
  ?domains:int -> ?trusted_proxies:(string -> bool) ->
  ?filter:(Req.t -> string Answer.t option) ->
  env:< net: _ Eio.Net.t; domain_mgr: _ Eio.Domain_manager.t; fs: _ Eio.Path.t; .. > ->
  sw:Eio.Switch.t -> (int * server_kind) list -> unit
