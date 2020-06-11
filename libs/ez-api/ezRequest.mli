
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
  internal error (-1 connection, -2 decoding) *)
type error_handler = (int -> string option -> unit)
module type RAWGEN = sig

  type ('output, 'error, 'security) service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'output, 'error, 'security) service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('input, 'output, 'error, 'security) post_service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'input, 'output, 'error, 'security) post_service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('output, 'error) reply_handler

  val get0 :
    EzAPI.base_url ->                   (* API url *)
    ('output, 'error, 'security) service0 -> (* GET service *)
    string ->                           (* debug msg *)
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->            (* unhandled error handler *)
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    (('output, 'error) reply_handler) -> (* reply handler *)
    unit ->                           (* trigger *)
    unit

  val get1 :
    EzAPI.base_url ->
    ('arg, 'output, 'error, 'security) service1 ->
    string ->
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    (('output, 'error) reply_handler) ->
    'arg ->
    unit

  val post0 :
    EzAPI.base_url ->                 (* API url *)
    ('input, 'output, 'error, 'security) post_service0 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    input:'input ->                           (* input *)
    (('output, 'error) reply_handler) -> (* reply handler *)
    unit

  val post1 :
    EzAPI.base_url ->                 (* API url *)
    ('arg, 'input, 'output, 'error, 'security) post_service1 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    input:'input ->                           (* input *)
    'arg ->
    (('output, 'error) reply_handler) -> (* reply handler *)
    unit

end

module type RAW = RAWGEN
  with type ('output, 'error, 'security) service0 :=
    ('output, 'error, 'security) EzAPI.service0
   and type ('arg, 'output, 'error, 'security) service1 :=
     ('arg, 'output, 'error, 'security) EzAPI.service1
   and type ('input, 'output, 'error, 'security) post_service0 :=
     ('input, 'output, 'error, 'security) EzAPI.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 :=
     ('arg, 'input, 'output, 'error, 'security) EzAPI.post_service1
   and type ('output, 'error) reply_handler := ('output, 'error) Result.result -> unit

module type LEGACY = RAWGEN
  with type ('output, 'error, 'security) service0 =
    ('output) EzAPI.Legacy.service0
   and type ('arg, 'output, 'error, 'security) service1 =
     ('arg, 'output) EzAPI.Legacy.service1
   and type ('input, 'output, 'error, 'security) post_service0 =
     ('input, 'output) EzAPI.Legacy.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 =
     ('arg, 'input, 'output) EzAPI.Legacy.post_service1
   and type ('output, 'error) reply_handler := 'output -> unit


(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  include RAW

  module Legacy : LEGACY

  val init : unit -> unit

  (* hook executed before every xhr *)
  val add_hook : (unit -> unit) -> unit

  val get :
    ?meth:Resto1.method_type ->
    string ->                 (* debug msg *)
    EzAPI.url ->              (* url *)
    ?headers:(string * string) list ->
    ?error:error_handler ->   (* error handler *)
    (string ->
     unit) ->       (* normal handler *)
    unit

  val post :
    ?meth:Resto1.method_type ->
    ?content_type:string ->
    ?content:string ->
    string ->
    EzAPI.url ->
    ?headers:(string * string) list ->
    ?error:error_handler ->
    (string -> unit) -> unit

end

val request_reply_hook : (unit -> unit) ref


type rep =
  | CodeOk of string
  | CodeError of int * string option


val log : (string -> unit) ref


(* Engine independent implementation. Beware: if you use these calls,
you must initialize an engine independantly.*)
module ANY : S

module Make(S : sig

    val xhr_get :
      ?meth:string ->
      string -> string ->
      ?headers:(string * string) list ->
      (rep -> unit) -> unit

    val xhr_post :
      ?meth:string ->
      ?content_type:string ->
      ?content:string ->
      string -> string ->
      ?headers:(string * string) list ->
      (rep -> unit) -> unit

  end) : S
