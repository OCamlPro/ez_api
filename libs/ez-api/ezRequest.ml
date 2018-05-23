open EzAPI.TYPES

type error_handler = (int -> unit)

module type S = sig

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

type rep =
  CodeOk of string
| CodeError of int

let xhr_get = ref (fun _msg _url ?headers:_ f -> f (CodeError (-1)))
let xhr_post = ref (fun ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f ->
                  f (CodeError (-1)))

let log = ref prerr_endline

let request_reply_hook = ref (fun () -> ())

(* print warnings generated when building the URL before
   sending the request *)
let internal_get msg (URL url) ?headers ?error f =
  EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
  !xhr_get msg url ?headers
   (fun code ->
     !request_reply_hook ();
     match code with
   | CodeOk res -> f res
   | CodeError n ->
      match error with
      | None -> ()
      | Some f -> f n)

let internal_post ?content_type ?content
         msg (URL url) ?headers ?error f =
  EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
  !xhr_post ?content_type ?content ?headers msg url
   (fun code ->
     !request_reply_hook ();
     match code with
     | CodeOk res -> f res
     | CodeError n ->
        match error with
        | None -> ()
        | Some f -> f n)

let before_xhr_hook = ref (fun () -> ())

let decode_result ?error encoding f res =
  match EzEncoding.destruct encoding res with
  | res -> f res
  | exception _exn ->
     match error with
     | None -> ()
     | Some error ->
        error (-2)

module ANY : S = struct
  let add_hook f =
    let old_hook = !before_xhr_hook in
    before_xhr_hook := (fun () -> old_hook (); f ())

  let get0 api
           ( service : 'output EzAPI.service0 )
           msg
           ?(post=false)
           ?headers
           ?error
           ?(params=[])
           f () =
    !before_xhr_hook ();
    let encoding = EzAPI.service_output service in
    if post then
      let url = EzAPI.forge0 api service [] in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post msg url ~content ~content_type ?headers ?error
                    (decode_result ?error encoding f)
    else
      let url = EzAPI.forge0 api service params in
      internal_get msg url ?headers ?error (decode_result ?error encoding f)

  let get1 api
           ( service : ('arg,'output) EzAPI.service1 )
           msg
           ?(post=false)
           ?headers
           ?error
           ?(params=[])
           f
           (arg : 'arg) =
    !before_xhr_hook ();
    let encoding = EzAPI.service_output service in
    if post then
      let url = EzAPI.forge1 api service arg []  in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post msg url ~content ~content_type ?headers ?error
                    (decode_result ?error encoding f)
    else
      let url = EzAPI.forge1 api service arg params in
      internal_get msg url ?headers ?error (decode_result ?error encoding f)

  let post0 api
            ( service : ('input,'output) EzAPI.post_service0 )
            msg
            ?headers
            ?error
            ?(params=[])
            ~(input : 'input)
            f
    =
    !before_xhr_hook ();
    let input_encoding = EzAPI.service_input service in
    let output_encoding = EzAPI.service_output service in
    let url = EzAPI.forge0 api service params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post msg url ~content ~content_type ?headers ?error
                  (decode_result ?error output_encoding f)

  let post1 api
            (service : ('arg, 'input,'output) EzAPI.post_service1 )
            msg
            ?headers
            ?error
            ?(params=[])
            ~(input : 'input)
            (arg : 'arg)
            f
    =
    !before_xhr_hook ();
    let input_encoding = EzAPI.service_input service in
    let output_encoding = EzAPI.service_output service in
    let url = EzAPI.forge1 api service arg params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post msg url ~content ~content_type ?headers ?error
                  (decode_result ?error output_encoding f)

  let get = internal_get
  let post = internal_post

end
