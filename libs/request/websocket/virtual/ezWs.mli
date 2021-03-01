include EzWsCommon.S

val connect :
  ?msg:string -> react:(string -> unit Lwt.t) -> string ->
  (ws_res, int * string option) result Lwt.t
