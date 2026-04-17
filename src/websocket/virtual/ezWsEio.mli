include EzWsEioTypes.S

val connect : ?msg:string -> ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Std.r ->
  sw:Eio.Switch.t ->
  react:(string action -> string -> unit r) ->
  string -> string ws r

val connect0 : ?msg:string -> ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Std.r ->
  sw:Eio.Switch.t ->
  react:('input action -> ('output, 'error) result -> unit r) ->
  EzAPI.base_url ->
  ('input, 'output, 'error, _) EzAPI.ws_service0 ->
  'input ws r
