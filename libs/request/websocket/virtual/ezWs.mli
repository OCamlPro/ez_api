include EzWsCommon.S

val connect :
  ?msg:string -> react:(string -> unit Lwt.t) -> string ->
  (string ws_res, int * string option) result Lwt.t

val connect0 :
  ?msg:string -> react:('output -> unit Lwt.t) -> EzAPI.base_url ->
  ('input, 'output, 'error, 'security) EzAPI.ws_service ->
  ('input ws_res, int * string option) result Lwt.t
