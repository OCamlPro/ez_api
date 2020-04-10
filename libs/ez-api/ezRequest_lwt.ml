open EzAPI.TYPES

let (>|=) = Lwt.(>|=)
let return = Lwt.return

type 'a api_result = ('a, (int * string option)) result

module type S = sig

val init : unit -> unit

val get0 :
  ?post:bool ->
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->                    (* debug msg *)
  EzAPI.base_url ->                 (* API url *)
  'output EzAPI.service0 ->         (* GET service *)
  'output api_result Lwt.t

val get1 :
  ?post:bool ->
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->
  EzAPI.base_url ->
  ('arg, 'output) EzAPI.service1 ->
  'arg ->
  'output api_result Lwt.t

val post0 :
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->
  input:'input ->                           (* input *)
  EzAPI.base_url ->                 (* API url *)
  ('input,'output) EzAPI.post_service0 -> (* POST service *)
  'output api_result Lwt.t

val post1 :
  ?headers:(string * string) list ->
  ?params:(EzAPI.param * EzAPI.arg_value) list ->
  ?msg:string ->
  input:'input ->                           (* input *)
  EzAPI.base_url ->                 (* API url *)
  ('arg, 'input,'output) EzAPI.post_service1 -> (* POST service *)
  'arg ->
  'output api_result Lwt.t

val get :
  ?headers:(string * string) list ->
  ?msg:string ->
  EzAPI.url ->              (* url *)
  string api_result Lwt.t

val post :
  ?content_type:string ->
  ?content:string ->
  ?headers:(string * string) list ->
  ?msg:string ->
  EzAPI.url ->
  string api_result Lwt.t

(* hook executed before every xhr *)
val add_hook : (unit -> unit) -> unit

end

let log = ref prerr_endline

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result encoding = function
  | Error _ as e -> e
  | Ok res ->
    match EzEncoding.destruct encoding res with
    | res -> (Ok res)
    | exception exn ->
      let msg = Printf.sprintf "Decoding error: %s in\n%s"
          (Printexc.to_string exn) res in
      (Error ((-2), (Some msg)))

let any_get = ref (fun ?headers:_ ?msg:_ _url -> return (Error (-1,None)))
let any_post = ref (fun ?content_type:(_x="") ?content:(_y="") ?headers:_ ?msg:_ _url ->
    return (Error (-1,None)))

module Make(S : sig

    val get :
      ?headers:(string * string) list ->
      ?msg:string -> string ->
      string api_result Lwt.t

    val post :
      ?content_type:string ->
      ?content:string ->
      ?headers:(string * string) list ->
      ?msg:string -> string ->
      string api_result Lwt.t

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
      api (service:'output EzAPI.service0) =
    !before_hook ();
    let encoding = EzAPI.service_output service in
    if post then
      let url = EzAPI.forge0 api service [] in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post ~content ~content_type ?headers ?msg url >|=
      (decode_result encoding)

    else
      let url = EzAPI.forge0 api service params in
      internal_get ?headers ?msg url >|= (decode_result encoding)

  let get1 ?(post=false) ?headers ?(params=[]) ?msg
      api (service : ('arg,'output) EzAPI.service1) (arg : 'arg) =
    !before_hook ();
    let encoding = EzAPI.service_output service in
    if post then
      let url = EzAPI.forge1 api service arg []  in
      let content = EzAPI.encode_args service url params in
      let content_type = EzUrl.content_type in
      internal_post ~content ~content_type ?headers ?msg url >|=
      (decode_result encoding)
    else
      let url = EzAPI.forge1 api service arg params in
      internal_get ?headers ?msg url >|= (decode_result encoding)

  let post0 ?headers ?(params=[]) ?msg ~(input : 'input)
      api (service : ('input,'output) EzAPI.post_service0) =
    !before_hook ();
    let input_encoding = EzAPI.service_input service in
    let output_encoding = EzAPI.service_output service in
    let url = EzAPI.forge0 api service params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post ~content ~content_type ?headers ?msg url >|=
    (decode_result output_encoding)

  let post1 ?headers ?(params=[]) ?msg ~(input : 'input)
      api (service : ('arg, 'input,'output) EzAPI.post_service1) (arg : 'arg) =
    !before_hook ();
    let input_encoding = EzAPI.service_input service in
    let output_encoding = EzAPI.service_output service in
    let url = EzAPI.forge1 api service arg params in
    let content = EzEncoding.construct input_encoding input in
    let content_type = "application/json" in
    internal_post ~content ~content_type ?headers ?msg url >|=
    (decode_result output_encoding)

  let get = internal_get
  let post = internal_post

end

module ANY : S = Make(struct
    let get ?headers ?msg url = !any_get ?headers ?msg url
    let post ?content_type ?content ?headers ?msg url =
      !any_post ?content_type ?content ?headers ?msg url
  end)

module Default = Make(struct
    let get ?headers:_ ?msg:_ _url = return (Error (-1,None))
    let post ?content_type:(_x="") ?content:(_y="") ?headers:_ ?msg:_ _url =
      return (Error (-1,None))
  end)

let () = Default.init ()
