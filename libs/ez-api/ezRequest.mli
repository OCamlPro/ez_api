
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
  internal error (-1 connection, -2 decoding) *)
type error_handler = (int -> string option -> unit)

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type SGen = sig

  val init : unit -> unit

  type ('output, 'error) service0
  type ('arg, 'output, 'error) service1
  type ('input, 'output, 'error) post_service0
  type ('arg, 'input, 'output, 'error) post_service1
  type ('output, 'error) reply_handler

val get0 :
  EzAPI.base_url ->                   (* API url *)
  ('output, 'error) service0 -> (* GET service *)
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
  ('arg, 'output, 'error) service1 ->
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
  ('input, 'output, 'error) post_service0 -> (* POST service *)
  string ->                         (* debug msg *)
  ?headers:(string * string) list ->
  ?error: error_handler ->          (* error handler *)
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  input:'input ->                           (* input *)
  (('output, 'error) reply_handler) -> (* reply handler *)
  unit

val post1 :
  EzAPI.base_url ->                 (* API url *)
  ('arg, 'input, 'output, 'error) post_service1 -> (* POST service *)
  string ->                         (* debug msg *)
  ?headers:(string * string) list ->
  ?error: error_handler ->          (* error handler *)
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  input:'input ->                           (* input *)
  'arg ->
  (('output, 'error) reply_handler) -> (* reply handler *)
  unit

val get :
  string ->                 (* debug msg *)
  EzAPI.url ->              (* url *)
  ?headers:(string * string) list ->
  ?error:error_handler ->   (* error handler *)
  (string ->
   unit) ->       (* normal handler *)
  unit

val post :
  ?content_type:string ->
  ?content:string ->
  string ->
  EzAPI.url ->
  ?headers:(string * string) list ->
  ?error:error_handler ->
  (string -> unit) -> unit

(* hook executed before every xhr *)
val add_hook : (unit -> unit) -> unit

end

val request_reply_hook : (unit -> unit) ref


type rep =
  | CodeOk of string
  | CodeError of int * string option


val log : (string -> unit) ref

module type S = SGen
  with type ('output, 'error) service0 :=
    ('output, 'error) EzAPI.service0
   and type ('arg, 'output, 'error) service1 :=
     ('arg, 'output, 'error) EzAPI.service1
   and type ('input, 'output, 'error) post_service0 :=
     ('input, 'output, 'error) EzAPI.post_service0
   and type ('arg, 'input, 'output, 'error) post_service1 :=
     ('arg, 'input, 'output, 'error) EzAPI.post_service1
   and type ('output, 'error) reply_handler := ('output, 'error) Result.result -> unit


(* Engine independent implementation. Beware: if you use these calls,
you must initialize an engine independantly.*)
module ANY : S

module Make(S : sig

    val xhr_get :
      string -> string ->
      ?headers:(string * string) list ->
      (rep -> unit) -> unit

    val xhr_post :
      ?content_type:string ->
      ?content:string ->
      string -> string ->
      ?headers:(string * string) list ->
      (rep -> unit) -> unit

  end) : S


module Legacy : sig

  module type S = SGen
    with type ('output, 'error) service0 :=
      ('output) EzAPI.Legacy.service0
     and type ('arg, 'output, 'error) service1 :=
       ('arg, 'output) EzAPI.Legacy.service1
     and type ('input, 'output, 'error) post_service0 :=
       ('input, 'output) EzAPI.Legacy.post_service0
     and type ('arg, 'input, 'output, 'error) post_service1 :=
       ('arg, 'input, 'output) EzAPI.Legacy.post_service1
     and type ('output, 'error) reply_handler := 'output -> unit

  module ANY : S

  module Make(S : sig

      val xhr_get :
        string -> string ->
        ?headers:(string * string) list ->
        (rep -> unit) -> unit

      val xhr_post :
        ?content_type:string ->
        ?content:string ->
        string -> string ->
        ?headers:(string * string) list ->
        (rep -> unit) -> unit

    end) : S

end
