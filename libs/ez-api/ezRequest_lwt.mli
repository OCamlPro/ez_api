
(* Note that `?content_type` in post can be overriden by a content-type
   header in `?headers` *)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
   internal error (-1 connection, -2 decoding) *)
type 'a api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknwownError of { code : int ; msg : string option }
type ('output, 'error) api_result = ('output, 'error api_error) result

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type SGen = sig

  type ('output, 'error) service0
  type ('arg, 'output, 'error) service1
  type ('input, 'output, 'error) post_service0
  type ('arg, 'input, 'output, 'error) post_service1
  type ('output, 'error) api_result

  val init : unit -> unit

  val get0 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->                    (* debug msg *)
    EzAPI.base_url ->                 (* API url *)
    ('output, 'error) service0 ->         (* GET service *)
    ('output, 'error) api_result Lwt.t

  val get1 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg: string ->
    EzAPI.base_url ->
    ('arg, 'output, 'error) service1 ->
    'arg ->
    ('output, 'error) api_result Lwt.t

  val post0 :
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->
    input:'input ->                           (* input *)
    EzAPI.base_url ->                 (* API url *)
    ('input,'output, 'error) post_service0 -> (* POST service *)
    ('output, 'error) api_result Lwt.t

  val post1 :
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ?msg:string ->
    input:'input ->                           (* input *)
    EzAPI.base_url ->                 (* API url *)
    ('arg, 'input,'output, 'error) post_service1 -> (* POST service *)
    'arg ->
    ('output, 'error) api_result Lwt.t

  val get :
    ?headers:(string * string) list ->
    ?msg:string ->
    EzAPI.url ->              (* url *)
    (string, int * string option) result Lwt.t

  val post :
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

module type S = SGen
  with type ('output, 'error) service0 :=
    ('output, 'error) EzAPI.service0
   and type ('arg, 'output, 'error) service1 :=
     ('arg, 'output, 'error) EzAPI.service1
   and type ('input, 'output, 'error) post_service0 :=
     ('input, 'output, 'error) EzAPI.post_service0
   and type ('arg, 'input, 'output, 'error) post_service1 :=
     ('arg, 'input, 'output, 'error) EzAPI.post_service1
   and type ('output, 'error) api_result := ('output, 'error) api_result

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)
module ANY : S

module Make(S : sig

    val get :
      ?headers:(string * string) list ->
      ?msg:string -> string ->
      (string, int * string option) result Lwt.t

    val post :
      ?content_type:string ->
      ?content:string ->
      ?headers:(string * string) list ->
      ?msg:string -> string ->
      (string, int * string option) result Lwt.t

  end) : S


module Legacy : sig

  type 'output api_result = ('output, (int * string option)) result

  module type S = SGen
    with type ('output, 'error) service0 :=
      ('output) EzAPI.Legacy.service0
     and type ('arg, 'output, 'error) service1 :=
       ('arg, 'output) EzAPI.Legacy.service1
     and type ('input, 'output, 'error) post_service0 :=
       ('input, 'output) EzAPI.Legacy.post_service0
     and type ('arg, 'input, 'output, 'error) post_service1 :=
       ('arg, 'input, 'output) EzAPI.Legacy.post_service1
     and type ('output, 'error) api_result := 'output api_result

  module ANY : S

  module Make(S : sig

      val get :
        ?headers:(string * string) list ->
        ?msg:string -> string ->
        (string, int * string option) result Lwt.t

      val post :
        ?content_type:string ->
        ?content:string ->
        ?headers:(string * string) list ->
        ?msg:string -> string ->
        (string, int * string option) result Lwt.t

    end) : S

end
