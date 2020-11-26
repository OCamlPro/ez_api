
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)
module type RAWGEN = sig

  type ('output, 'error, 'security) service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'output, 'error, 'security) service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('input, 'output, 'error, 'security) post_service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'input, 'output, 'error, 'security) post_service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('output, 'error) api_result

  val get0 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->                    (* debug msg *)
    EzAPI.base_url ->                 (* API url *)
    ('output, 'error, 'security) service0 ->         (* GET service *)
    ('output, 'error) api_result Lwt.t

  val get1 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg: string ->
    EzAPI.base_url ->
    ('arg, 'output, 'error, 'security) service1 ->
    'arg ->
    ('output, 'error) api_result Lwt.t

  val post0 :
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->
    input:'input ->                           (* input *)
    EzAPI.base_url ->                 (* API url *)
    ('input,'output, 'error, 'security) post_service0 -> (* POST service *)
    ('output, 'error) api_result Lwt.t

  val post1 :
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->
    input:'input ->                           (* input *)
    EzAPI.base_url ->                 (* API url *)
    ('arg, 'input,'output, 'error, 'security) post_service1 -> (* POST service *)
    'arg ->
    ('output, 'error) api_result Lwt.t

end

type 'a api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknownError of { code : int ; msg : string option }
type ('output, 'error) api_result = ('output, 'error api_error) result

val handle_error : ('a -> string option) -> 'a api_error -> int * string option
val string_of_error : ('a -> string option) -> 'a api_error -> string

module type RAW = RAWGEN
  with type ('output, 'error, 'security) service0 :=
    ('output, 'error, 'security) EzAPI.service0
   and type ('arg, 'output, 'error, 'security) service1 :=
     ('arg, 'output, 'error, 'security) EzAPI.service1
   and type ('input, 'output, 'error, 'security) post_service0 :=
     ('input, 'output, 'error, 'security) EzAPI.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 :=
     ('arg, 'input, 'output, 'error, 'security) EzAPI.post_service1
   and type ('output, 'error) api_result := ('output, 'error) api_result

module type LEGACY = RAWGEN
  with type ('output, 'error, 'security) service0 =
    ('output) EzAPI.Legacy.service0
   and type ('arg, 'output, 'error, 'security) service1 =
     ('arg, 'output) EzAPI.Legacy.service1
   and type ('input, 'output, 'error, 'security) post_service0 =
     ('input, 'output) EzAPI.Legacy.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 =
     ('arg, 'input, 'output) EzAPI.Legacy.post_service1
   and type ('output, 'error) api_result :=
     ('output, (int * string option)) result

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  include RAW

  module Legacy : LEGACY

  val init : unit -> unit

  val get :
    ?meth:Resto1.method_type ->
    ?headers:(string * string) list ->
    ?msg:string ->
    EzAPI.url ->              (* url *)
    (string, int * string option) result Lwt.t

  val post :
    ?meth:Resto1.method_type ->
    ?content_type:string ->
    ?content:string ->
    ?headers:(string * string) list ->
    ?msg:string ->
    EzAPI.url ->
    (string, int * string option) result Lwt.t

  (* hook executed before every xhr *)
  val add_hook : (unit -> unit) -> unit

end

val request_reply_hook : (unit -> unit) ref

val log : (string -> unit) ref

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)
module ANY : S

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

module Make(_ : Interface) : S
