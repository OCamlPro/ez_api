type error_handler = (int -> string option -> unit)

module type RAWGEN = sig

  type ('output, 'error, 'security) service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'output, 'error, 'security) service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg1, 'arg2, 'output, 'error, 'security) service2
    constraint 'security = [< EzAPI.security_scheme ]
  type ('input, 'output, 'error, 'security) post_service0
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg, 'input, 'output, 'error, 'security) post_service1
    constraint 'security = [< EzAPI.security_scheme ]
  type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2
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

  val get2 :
    EzAPI.base_url ->
    ('arg1, 'arg2, 'output, 'error, 'security) service2 ->
    string ->
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    (('output, 'error) reply_handler) ->
    'arg1 -> 'arg2 ->
    unit

  val post0 :
    EzAPI.base_url ->                 (* API url *)
    ('input, 'output, 'error, 'security) post_service0 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?url_encode:bool ->
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
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    'arg ->
    (('output, 'error) reply_handler) -> (* reply handler *)
    unit

  val post2 :
    EzAPI.base_url ->                 (* API url *)
    ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    'arg1 -> 'arg2 ->
    (('output, 'error) reply_handler) -> (* reply handler *)
    unit

end

module type RAW = RAWGEN
  with type ('output, 'error, 'security) service0 :=
    ('output, 'error, 'security) EzAPI.service0
   and type ('arg, 'output, 'error, 'security) service1 :=
     ('arg, 'output, 'error, 'security) EzAPI.service1
   and type ('arg1, 'arg2, 'output, 'error, 'security) service2 :=
     ('arg1, 'arg2, 'output, 'error, 'security) EzAPI.service2
   and type ('input, 'output, 'error, 'security) post_service0 :=
     ('input, 'output, 'error, 'security) EzAPI.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 :=
     ('arg, 'input, 'output, 'error, 'security) EzAPI.post_service1
   and type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 :=
     ('arg1, 'arg2, 'input, 'output, 'error, 'security) EzAPI.post_service2
   and type ('output, 'error) reply_handler := ('output, 'error) Result.result -> unit

module type LEGACY = RAWGEN
  with type ('output, 'error, 'security) service0 =
    ('output) EzAPI.Legacy.service0
   and type ('arg, 'output, 'error, 'security) service1 =
     ('arg, 'output) EzAPI.Legacy.service1
   and type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
     ('arg1, 'arg2, 'output) EzAPI.Legacy.service2
   and type ('input, 'output, 'error, 'security) post_service0 =
     ('input, 'output) EzAPI.Legacy.post_service0
   and type ('arg, 'input, 'output, 'error, 'security) post_service1 =
     ('arg, 'input, 'output) EzAPI.Legacy.post_service1
   and type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 =
     ('arg1, 'arg2, 'input, 'output) EzAPI.Legacy.post_service2
   and type ('output, 'error) reply_handler := 'output -> unit


(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  include RAW

  module Legacy : LEGACY

  val init : unit -> unit

  (* hook executed before every request *)
  val add_hook : (unit -> unit) -> unit
  (* hook executed after every request *)
  val add_reply_hook : (unit -> unit) -> unit

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

module type Interface = sig
  val get :
    ?meth:string ->
    string -> string ->
    ?headers:(string * string) list ->
    ((string, int * string option) result -> unit) -> unit

  val post :
    ?meth:string ->
    ?content_type:string ->
    ?content:string ->
    string -> string ->
    ?headers:(string * string) list ->
    ((string, int * string option) result -> unit) -> unit
end
