include EzWsCommon.S

val connect :
  ?msg:string ->
  ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  react:(string action -> string -> unit rp) ->
  string ->
  string ws rp

val connect0 :
  ?msg:string ->
  ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  react:('input action -> ('output, 'error) result -> unit rp) ->
  EzAPI.base_url ->
  ('input, 'output, 'error, _) EzAPI.ws_service0 ->
  'input ws rp
