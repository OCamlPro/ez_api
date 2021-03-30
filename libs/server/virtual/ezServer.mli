val server : ?catch:(string -> exn -> string EzAPIServerUtils.Answer.t Lwt.t) ->
  (int * EzAPIServerUtils.server_kind) list -> unit Lwt.t

val set_debug : unit -> unit
