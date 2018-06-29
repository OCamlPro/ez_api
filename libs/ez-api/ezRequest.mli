
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
  internal error (-1 connection, -2 decoding) *)
type error_handler = (int -> unit)

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  val init : unit -> unit

val get0 :
  EzAPI.base_url ->                 (* API url *)
  'output EzAPI.service0 ->         (* GET service *)
  string ->                         (* debug msg *)
  ?post:bool ->
  ?headers:(string * string) list ->
  ?error: error_handler ->          (* error handler *)
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ('output -> unit) ->              (* reply handler *)
  unit ->                           (* trigger *)
  unit

val get1 :
  EzAPI.base_url ->
  ('arg, 'output) EzAPI.service1 ->
  string ->
  ?post:bool ->
  ?headers:(string * string) list ->
  ?error: error_handler ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ('output -> unit) ->
  'arg ->
  unit

val post0 :
  EzAPI.base_url ->                 (* API url *)
  ('input,'output) EzAPI.post_service0 -> (* POST service *)
  string ->                         (* debug msg *)
  ?headers:(string * string) list ->
  ?error: error_handler ->          (* error handler *)
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  input:'input ->                           (* input *)
  ('output -> unit) ->              (* reply handler *)
  unit

val post1 :
  EzAPI.base_url ->                 (* API url *)
  ('arg, 'input,'output) EzAPI.post_service1 -> (* POST service *)
  string ->                         (* debug msg *)
  ?headers:(string * string) list ->
  ?error: error_handler ->          (* error handler *)
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  input:'input ->                           (* input *)
  'arg ->
  ('output -> unit) ->              (* reply handler *)
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
  | CodeError of int


val log : (string -> unit) ref

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
