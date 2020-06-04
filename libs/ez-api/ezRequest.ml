open EzAPI.TYPES

type error_handler = (int -> string option -> unit)

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

type rep =
  CodeOk of string
| CodeError of int * string option

let log = ref prerr_endline

let request_reply_hook = ref (fun () -> ())

let before_xhr_hook = ref (fun () -> ())

let decode_result ?error encoding f res =
  match EzEncoding.destruct encoding res with
  | res -> f res
  | exception exn ->
     match error with
     | None -> ()
     | Some error ->
       let msg = Printf.sprintf "Decoding error: %s in\n%s"
           (Printexc.to_string exn) res in
        error (-2) (Some msg)


let any_xhr_get = ref (fun _msg _url ?headers:_ f -> f (CodeError (-1,None)))
let any_xhr_post = ref (fun ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f ->
                  f (CodeError (-1,None)))

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

    end) = struct

  let init () =
    any_xhr_get := S.xhr_get;
    any_xhr_post := S.xhr_post;
    ()

  let () = init ()

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    S.xhr_get msg url ?headers
      (fun code ->
         !request_reply_hook ();
         match code with
         | CodeOk res -> f res
         | CodeError (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let internal_post ?content_type ?content
      msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    S.xhr_post ?content_type ?content ?headers msg url
      (fun code ->
         !request_reply_hook ();
         match code with
         | CodeOk res -> f res
         | CodeError (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let add_hook f =
    let old_hook = !before_xhr_hook in
    before_xhr_hook := (fun () -> old_hook (); f ())

  let handlers ?error service f =
    let error code msg =
      match EzAPI.service_errors service ~code, msg with
      | Some enc, Some s ->
        decode_result ?error enc (fun x -> f (Error x)) s
      | _, _ -> match error with
        | Some error -> error code msg (* unhandled *)
        | None -> ()
    in
    let encoding = EzAPI.service_output service in
    let ok = decode_result ~error encoding (fun x -> f (Ok x)) in
    ok, error

  let get0 api
           ( service : ('output, 'error) EzAPI.service0 )
           msg
           ?(post=false)
           ?headers
           ?error
           ?(params=[])
           f () =
    !before_xhr_hook ();
    let ok, error = handlers ?error service f in
    if post then
      let url = EzAPI.forge0 api service [] in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post msg url ~content ~content_type ?headers ~error ok
    else
      let url = EzAPI.forge0 api service params in
      internal_get msg url ?headers ~error ok

  let get1 api
           ( service : ('arg, 'output, 'error) EzAPI.service1 )
           msg
           ?(post=false)
           ?headers
           ?error
           ?(params=[])
           f
           (arg : 'arg) =
    !before_xhr_hook ();
    let ok, error = handlers ?error service f in
    if post then
      let url = EzAPI.forge1 api service arg []  in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post msg url ~content ~content_type ?headers ~error ok
    else
      let url = EzAPI.forge1 api service arg params in
      internal_get msg url ?headers ~error ok

  let post0 api
            ( service : ('input, 'output, 'error) EzAPI.post_service0 )
            msg
            ?headers
            ?error
            ?(params=[])
            ~(input : 'input)
            f
    =
    !before_xhr_hook ();
    let ok, error = handlers ?error service f in
    let input_encoding = EzAPI.service_input service in
    let url = EzAPI.forge0 api service params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post msg url ~content ~content_type ?headers ~error ok

  let post1 api
            (service : ('arg, 'input, 'output, 'error) EzAPI.post_service1 )
            msg
            ?headers
            ?error
            ?(params=[])
            ~(input : 'input)
            (arg : 'arg)
            f
    =
    !before_xhr_hook ();
    let ok, error = handlers ?error service f in
    let input_encoding = EzAPI.service_input service in
    let url = EzAPI.forge1 api service arg params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post msg url ~content ~content_type ?headers ~error ok

  let get = internal_get
  let post = internal_post

end

module ANY : S = Make(struct
    let xhr_get msg url ?headers f =
      !any_xhr_get msg url ?headers f
    let xhr_post ?content_type ?content msg url ?headers f =
      !any_xhr_post ?content_type ?content msg url ?headers f
    end)

module Default = Make(struct

    let xhr_get _msg _url ?headers:_ f = f (CodeError (-1,None))
    let xhr_post ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f
      =
      f (CodeError (-1,None))

    end)

let () = Default.init ()


module Legacy = struct

  module type NewS = S

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

  module Unresultize(R : NewS) : S = struct

    include R
    open EzAPI.Legacy

    let unresultize f = function
      | Ok res -> f res
      | Error u -> unreachable u

    let get0 api ( service : 'output service0 )
        msg ?post ?headers ?error ?params f () =
      get0 api service msg ?post ?headers ?error ?params (unresultize f) ()

    let get1 api ( service : ('arg, 'output) service1 )
        msg ?post ?headers ?error ?params f arg =
      get1 api service msg ?post ?headers ?error ?params (unresultize f) arg

    let post0 api ( service : ('input, 'output) post_service0 )
        msg ?headers ?error ?params ~input f =
      post0 api service msg ?headers ?error ?params ~input (unresultize f)

    let post1 api (service : ('arg, 'input, 'output) post_service1 )
        msg ?headers ?error ?params ~input arg f =
      post1 api service msg ?headers ?error ?params ~input arg (unresultize f)

  end

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

    end) = Unresultize(Make(S))

  module ANY : S = Unresultize(ANY)

end
