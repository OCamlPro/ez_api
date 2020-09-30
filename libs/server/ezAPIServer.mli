val server : ?require_method:bool -> ?catch:(string -> exn -> (int * EzAPIServerUtils.reply) Lwt.t) ->
  (int * EzAPIServerUtils.server_kind) list -> unit Lwt.t
