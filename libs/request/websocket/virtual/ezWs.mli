include EzWsCommon.S

val connect :
  ?msg:string ->
  react:((string -> unit rp) -> string -> unit rp) ->
  string ->
  string ws rp


val connect0 :
  ?msg:string ->
  react:(('input -> unit rp) -> ('output, 'error) result -> unit rp) ->
  EzAPI.base_url ->
  ('input, 'output, 'error, _) EzAPI.ws_service0 ->
  'input ws rp
