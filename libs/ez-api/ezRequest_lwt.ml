open EzAPI.TYPES

let (>|=) = Lwt.(>|=)
let return = Lwt.return

type 'a api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknwownError of { code : int ; msg : string option }
type ('output, 'error) api_result = ('output, 'error api_error) result

module type S = sig

val init : unit -> unit

val get0 :
  ?post:bool ->
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->                    (* debug msg *)
  EzAPI.base_url ->                 (* API url *)
  ('output, 'error) EzAPI.service0 ->         (* GET service *)
  ('output, 'error) api_result Lwt.t

val get1 :
  ?post:bool ->
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg: string ->
  EzAPI.base_url ->
  ('arg, 'output, 'error) EzAPI.service1 ->
  'arg ->
  ('output, 'error) api_result Lwt.t

val post0 :
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->
  input:'input ->                           (* input *)
  EzAPI.base_url ->                 (* API url *)
  ('input,'output, 'error) EzAPI.post_service0 -> (* POST service *)
  ('output, 'error) api_result Lwt.t

val post1 :
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->
  input:'input ->                           (* input *)
  EzAPI.base_url ->                 (* API url *)
  ('arg, 'input,'output, 'error) EzAPI.post_service1 -> (* POST service *)
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

let log = ref prerr_endline

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result encoding err_encodings = function
  | Error (code, None) -> Error (UnknwownError { code ; msg = None })
  | Error (code, Some msg) ->
    (match err_encodings ~code with
      | None -> Error (UnknwownError { code ; msg = Some msg })
      | Some encoding ->
        try Error (
            KnownError { code ; error = EzEncoding.destruct encoding msg })
        with _ -> Error (UnknwownError { code ; msg = Some msg })
    )
  | Ok res ->
    match EzEncoding.destruct encoding res with
    | res -> (Ok res)
    | exception exn ->
      let msg = Printf.sprintf "Decoding error: %s in\n%s"
          (Printexc.to_string exn) res in
      Error (UnknwownError { code = -2; msg = Some msg })

let handle_result service res =
  let err_encodings = EzAPI.service_errors service in
  let encoding = EzAPI.service_output service in
  decode_result encoding err_encodings res

let any_get = ref (fun ?headers:_ ?msg:_ _url ->
    return (Error (-2, None))
  )
let any_post = ref (fun ?content_type:(_x="") ?content:(_y="") ?headers:_ ?msg:_ _url ->
    return (Error (-2, None))
  )

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

    end) = struct

  let init () =
    any_get := S.get;
    any_post := S.post;
    ()

  let () = init ()

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    S.get ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let internal_post ?content_type ?content ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf !log "EzRequest.warning: %s" s);
    S.post ?content_type ?content ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let add_hook f =
    let old_hook = !before_hook in
    before_hook := (fun () -> old_hook (); f ())

  let get0 ?(post=false) ?headers ?(params=[]) ?msg
      api (service: ('output, 'error) EzAPI.service0) =
    !before_hook ();
    if post then
      let url = EzAPI.forge0 api service [] in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post ~content ~content_type ?headers ?msg url >|=
      handle_result service
    else
      let url = EzAPI.forge0 api service params in
      internal_get ?headers ?msg url >|= handle_result service

  let get1 ?(post=false) ?headers ?(params=[]) ?msg
      api (service : ('arg,'output, 'error) EzAPI.service1) (arg : 'arg) =
    !before_hook ();
    if post then
      let url = EzAPI.forge1 api service arg []  in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post ~content ~content_type ?headers ?msg url >|=
      handle_result service
    else
      let url = EzAPI.forge1 api service arg params in
      internal_get ?headers ?msg url >|= handle_result service

  let post0 ?headers ?(params=[]) ?msg ~(input : 'input)
      api (service : ('input,'output, 'error) EzAPI.post_service0) =
    !before_hook ();
    let input_encoding = EzAPI.service_input service in
    let url = EzAPI.forge0 api service params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post ~content ~content_type ?headers ?msg url >|=
    handle_result service

  let post1 ?headers ?(params=[]) ?msg ~(input : 'input)
      api (service : ('arg, 'input,'output, 'error) EzAPI.post_service1) (arg : 'arg) =
    !before_hook ();
    let input_encoding = EzAPI.service_input service in
    let url = EzAPI.forge1 api service arg params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post ~content ~content_type ?headers ?msg url >|=
    handle_result service

  let get = internal_get
  let post = internal_post

end

module ANY : S = Make(struct
    let get ?headers ?msg url = !any_get ?headers ?msg url
    let post ?content_type ?content ?headers ?msg url =
      !any_post ?content_type ?content ?headers ?msg url
  end)

module Default = Make(struct
    let get ?headers:_ ?msg:_ _url = return (Error (-2, None))
    let post ?content_type:(_x="") ?content:(_y="") ?headers:_ ?msg:_ _url =
      return (Error (-2, None))
  end)

let () = Default.init ()
