open EzAPI.TYPES

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

module type S = sig

  include RAW

  module Legacy : LEGACY

  val init : unit -> unit

  (* hook executed before every request *)
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

let log = ref prerr_endline

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result ?error encoding f res =
  let res = match res with "" -> "{}" | res -> res in
  match EzEncoding.destruct encoding res with
  | res -> f res
  | exception exn ->
     match error with
     | None -> ()
     | Some error ->
       let msg = Printf.sprintf "Decoding error: %s in\n%s"
           (Printexc.to_string exn) res in
        error (-3) (Some msg)


let any_get = ref (fun ?meth:_m _msg _url ?headers:_ f ->
    f (Error (-2,Some "No http client loaded")))
let any_post = ref (fun ?meth:_m ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f ->
    f (Error (-2,Some "No http client loaded")))

module Make(S : sig

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

    end) = struct

  let init () =
    any_get := S.get;
    any_post := S.post;
    ()

  let () = init ()

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?meth msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m ->
      Some (String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.get ?meth msg url ?headers
      (fun code ->
         !request_reply_hook ();
         match code with
         | Ok res -> f res
         | Error (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let internal_post ?meth ?content_type ?content
      msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.post ?meth ?content_type ?content ?headers msg url
      (fun code ->
         !request_reply_hook ();
         match code with
         | Ok res -> f res
         | Error (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let add_hook f =
    let old_hook = !before_hook in
    before_hook := (fun () -> old_hook (); f ())

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

  let get = internal_get
  let post = internal_post

  module Raw = struct

    let get0 api
        ( service : ('output, 'error, 'security) EzAPI.service0 )
        msg
        ?(post=false)
        ?headers
        ?error
        ?(params=[])
        f () =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge0 api service [] in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        let meth = if meth = Resto1.GET then Resto1.POST else meth in
        internal_post ~meth msg url ~content ~content_type ?headers ~error ok
      else
        let url = EzAPI.forge0 api service params in
        internal_get ~meth msg url ?headers ~error ok

    let get1 api
        ( service : ('arg, 'output, 'error, 'security) EzAPI.service1 )
        msg
        ?(post=false)
        ?headers
        ?error
        ?(params=[])
        f
        (arg : 'arg) =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge1 api service arg []  in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        let meth = if meth = Resto1.GET then Resto1.POST else meth in
        internal_post ~meth msg url ~content ~content_type ?headers ~error ok
      else
        let url = EzAPI.forge1 api service arg params in
        internal_get ~meth msg url ?headers ~error ok

    let post0 api
        ( service : ('input, 'output, 'error, 'security) EzAPI.post_service0 )
        msg
        ?headers
        ?error
        ?(params=[])
        ~(input : 'input)
        f
      =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      let input_encoding = EzAPI.service_input service in
      let url = EzAPI.forge0 api service params in
      let content = EzEncoding.construct input_encoding input in
      let content_type = "application/json" in
      internal_post ~meth msg url ~content ~content_type ?headers ~error ok

    let post1 api
        (service : ('arg, 'input, 'output, 'error, 'security) EzAPI.post_service1 )
        msg
        ?headers
        ?error
        ?(params=[])
        ~(input : 'input)
        (arg : 'arg)
        f
      =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      let input_encoding = EzAPI.service_input service in
      let url = EzAPI.forge1 api service arg params in
      let content = EzEncoding.construct input_encoding input in
      let content_type = "application/json" in
      internal_post msg ~meth url ~content ~content_type ?headers ~error ok
  end

  include Raw

  module Legacy = struct

    type ('output, 'error, 'security) service0 =
      ('output) EzAPI.Legacy.service0
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg, 'output, 'error, 'security) service1 =
      ('arg, 'output) EzAPI.Legacy.service1
      constraint 'security = [< EzAPI.security_scheme ]

    type ('input, 'output, 'error, 'security) post_service0 =
      ('input, 'output) EzAPI.Legacy.post_service0
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg, 'input, 'output, 'error, 'security) post_service1 =
      ('arg, 'input, 'output) EzAPI.Legacy.post_service1
      constraint 'security = [< EzAPI.security_scheme ]

    let unresultize f = function
      | Ok res -> f res
      | Error u -> EzAPI.unreachable u

    let get0 api service
        msg ?post ?headers ?error ?params f () =
      Raw.get0 api service msg ?post ?headers ?error ?params (unresultize f) ()

    let get1 api service
        msg ?post ?headers ?error ?params f arg =
      Raw.get1 api service msg ?post ?headers ?error ?params (unresultize f) arg

    let post0 api service
        msg ?headers ?error ?params ~input f =
      Raw.post0 api service msg ?headers ?error ?params ~input (unresultize f)

    let post1 api service
        msg ?headers ?error ?params ~input arg f =
      Raw.post1 api service msg ?headers ?error ?params ~input arg (unresultize f)

  end

end

module ANY : S = Make(struct
    let get ?meth msg url ?headers f =
      !any_get ?meth msg url ?headers f
    let post ?meth ?content_type ?content msg url ?headers f =
      !any_post ?meth ?content_type ?content msg url ?headers f
  end)

module Default = Make(struct

    let get ?meth:_meth _msg _url ?headers:_ f = f (Error (-2,Some "No http client loaded"))
    let post ?meth:_meth ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f =
      f (Error (-2,Some "No http client loaded"))

  end)

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


let () = Default.init ()
