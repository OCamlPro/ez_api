open EzAPI

type headers = (string * string) list
type response = int * string * headers
type 'a error = int * [ `known of 'a | `unknown of string option ]

type net_ty = [ `Network | `Platform of [ `Generic | `Unix ] ]
type 'a net = ([> net_ty] as 'a) Eio.Resource.t

module type S = sig

  val get: ?meth:Meth.t -> ?headers:(string * string) list -> ?msg:string ->
    net:_ net -> sw:Eio.Switch.t -> url -> response

  val post: ?meth:Meth.t -> ?headers:(string * string) list -> ?msg:string ->
    ?content_type:string -> ?content:string ->
    net:_ net -> sw:Eio.Switch.t -> url -> response

  val get0: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg:string -> ?post:bool -> net:_ net -> sw:Eio.Switch.t ->
    base_url -> ('output, 'error, 'security) service0 ->
    ('output, 'error error) result

  val get1: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg: string -> ?post:bool -> net:_ net -> sw:Eio.Switch.t ->
    base_url -> ('a, 'output, 'error, 'security) service1 -> 'a ->
    ('output, 'error error) result

  val get2: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg: string -> ?post:bool -> net:_ net -> sw:Eio.Switch.t ->
    base_url -> ('a1, 'a2, 'output, 'error, 'security) service2 -> 'a1 -> 'a2 ->
    ('output, 'error error) result

  val post0: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg:string -> ?url_encode:bool -> net:_ net -> sw:Eio.Switch.t -> input:'input ->
    base_url -> ('input, 'output, 'error, 'security) post_service0 ->
    ('output, 'error error) result

  val post1: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg:string -> ?url_encode:bool -> net:_ net -> sw:Eio.Switch.t -> input:'input ->
    base_url -> ('a, 'input, 'output, 'error, 'security) post_service1 -> 'a ->
    ('output, 'error error) result

  val post2: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg:string -> ?url_encode:bool -> net:_ net -> sw:Eio.Switch.t -> input:'input ->
    base_url -> ('a1, 'a2, 'input,'output, 'error, 'security) post_service2 -> 'a1 -> 'a2 ->
    ('output, 'error error) result

  val request: ?headers:(string * string) list -> ?params:(Param.t * param_value) list ->
    ?msg:string -> ?post:bool -> ?url_encode:bool -> net:_ net -> sw:Eio.Switch.t -> input:'input ->
    base_url -> ('a, 'input,'output, 'error, 'security) service -> 'a ->
    ('output, 'error error) result
end

module type Interface = sig

  val get: ?meth:Meth.t -> ?headers:(string * string) list -> ?msg:string ->
    net:_ net -> sw:Eio.Switch.t -> string -> response

  val post : ?meth:Meth.t -> ?headers:(string * string) list -> ?msg:string ->
    ?content_type:string -> ?content:string -> net:_ net -> sw:Eio.Switch.t ->
    string -> response
end
