open EzAPI

type 'a api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknownError of { code : int ; msg : string option }
type ('output, 'error) api_result = ('output, 'error api_error) result

(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)
module type RAWGEN = sig

  type ('output, 'error, 'security) service0
    constraint 'security = [< Security.scheme ]
  type ('arg, 'output, 'error, 'security) service1
    constraint 'security = [< Security.scheme ]
  type ('arg1, 'arg2, 'output, 'error, 'security) service2
    constraint 'security = [< Security.scheme ]
  type ('input, 'output, 'error, 'security) post_service0
    constraint 'security = [< Security.scheme ]
  type ('arg, 'input, 'output, 'error, 'security) post_service1
    constraint 'security = [< Security.scheme ]
  type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2
    constraint 'security = [< Security.scheme ]
  type ('output, 'error) api_result

  val get0 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->                    (* debug msg *)
    base_url ->                 (* API url *)
    ('output, 'error, 'security) service0 ->         (* GET service *)
    ('output, 'error) api_result Lwt.t

  val get1 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg: string ->
    base_url ->
    ('arg, 'output, 'error, 'security) service1 ->
    'arg ->
    ('output, 'error) api_result Lwt.t

  val get2 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg: string ->
    base_url ->
    ('arg1, 'arg2, 'output, 'error, 'security) service2 ->
    'arg1 -> 'arg2 ->
    ('output, 'error) api_result Lwt.t

  val post0 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('input,'output, 'error, 'security) post_service0 -> (* POST service *)
    ('output, 'error) api_result Lwt.t

  val post1 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('arg, 'input,'output, 'error, 'security) post_service1 -> (* POST service *)
    'arg ->
    ('output, 'error) api_result Lwt.t

  val post2 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('arg1, 'arg2, 'input,'output, 'error, 'security) post_service2 -> (* POST service *)
    'arg1 -> 'arg2 ->
    ('output, 'error) api_result Lwt.t

end

module type RAW = RAWGEN
  with type ('output, 'error, 'security) service0 :=
    ('output, 'error, 'security) service0
   and type ('arg, 'output, 'error, 'security) service1 :=
     ('arg, 'output, 'error, 'security) service1
   and type ('arg1, 'arg2, 'output, 'error, 'security) service2 :=
     ('arg1, 'arg2, 'output, 'error, 'security) service2
   and type ('input, 'output, 'error, 'security) post_service0 :=
     ('input, 'output, 'error, 'security) post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 :=
     ('arg, 'input, 'output, 'error, 'security) post_service1
   and type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 :=
     ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2
   and type ('output, 'error) api_result := ('output, 'error) api_result

module type LEGACY = RAWGEN
  with type ('output, 'error, 'security) service0 =
    ('output) Legacy.service0
   and type ('arg, 'output, 'error, 'security) service1 =
     ('arg, 'output) Legacy.service1
   and type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
     ('arg1, 'arg2, 'output) Legacy.service2
   and type ('input, 'output, 'error, 'security) post_service0 =
     ('input, 'output) Legacy.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 =
     ('arg, 'input, 'output) Legacy.post_service1
   and type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 =
     ('arg1, 'arg2, 'input, 'output) Legacy.post_service2
   and type ('output, 'error) api_result :=
     ('output, (int * string option)) result

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  include RAW

  module Legacy : LEGACY

  val init : unit -> unit

  val get :
    ?meth:Meth.all ->
    ?headers:(string * string) list ->
    ?msg:string ->
    url ->              (* url *)
    (string, int * string option) result Lwt.t

  val post :
    ?meth:Meth.all ->
    ?content_type:string ->
    ?content:string ->
    ?headers:(string * string) list ->
    ?msg:string ->
    url ->
    (string, int * string option) result Lwt.t

  (* hook executed before every xhr *)
  val add_hook : (unit -> unit) -> unit
  (* hook executed after every request *)
  val add_reply_hook : (unit -> unit) -> unit

end

module type Interface = sig

  val get :
    ?meth:string ->
    ?headers:(string * string) list ->
    ?msg:string -> string ->
    (string, int * string option) result Lwt.t

  val post :
    ?meth:string ->
    ?content_type:string ->
    ?content:string ->
    ?headers:(string * string) list ->
    ?msg:string -> string ->
    (string, int * string option) result Lwt.t

end
