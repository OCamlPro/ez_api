open EzAPI

type error_handler = (int -> string option -> unit)

module type S = EzReq_S.S
module type Interface = EzReq_S.Interface

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result ?error io f res =
  match IO.from_string io f res with
  | Ok () -> ()
  | Error e -> match error with
    | None -> ()
    | Some error -> error (-3) (Some (EzEncoding.error_to_string ~from:res e))

let any_get = ref (fun ?meth:_m ?headers:_ ?msg:_ _url f ->
    f (Error (-2,Some "No http client loaded")))
let any_post = ref (fun ?meth:_m ?content_type:(_x="") ?content:(_y="") ?headers:_ ?msg:_ _url f ->
    f (Error (-2,Some "No http client loaded")))

module Make(S : Interface) : S = struct

  let init () =
    any_get := S.get;
    any_post := S.post

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?meth ?headers ?msg ?error (URL url) f =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m ->
      Some (String.uppercase_ascii @@ Meth.to_string m) in
    S.get ?meth ?msg url ?headers
      (fun code ->
         !request_reply_hook ();
         match code with
         | Ok res -> f res
         | Error (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let internal_post ?meth ?content_type ?content ?headers ?msg ?error (URL url) f =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ Meth.to_string m) in
    S.post ?meth ?content_type ?content ?headers ?msg url
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

  let add_reply_hook f =
    let old_hook = !request_reply_hook in
    request_reply_hook := (fun () -> old_hook (); f ())

  let handlers ?error service f =
    let error code msg =
      match Service.error service ~code, msg with
      | Some enc, Some s ->
        decode_result ?error (Json enc) (fun x -> f (Error x)) s
      | _, _ -> match error with
        | Some error -> error code msg (* unhandled *)
        | None -> ()
    in
    let encoding = Service.output service in
    let ok = decode_result ~error encoding (fun x -> f (Ok x)) in
    ok, error

  let get = internal_get
  let post = internal_post

  module Raw = struct

    let get0 ?(post=false) ?headers ?(params=[]) ?msg ?error api service f =
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      if post then
        let url = forge0 api service [] in
        let content = encode_params service.s params in
        let content_type = Url.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth ?msg url ~content ~content_type ?headers ~error ok
      else
        let url = forge0 api service params in
        internal_get ~meth ?msg url ?headers ~error ok

    let get1 ?(post=false) ?headers ?(params=[]) ?msg ?error api service arg f =
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      if post then
        let url = forge1 api service arg []  in
        let content = encode_params service.s params in
        let content_type = Url.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth ?msg url ~content ~content_type ?headers ~error ok
      else
        let url = forge1 api service arg params in
        internal_get ~meth ?msg url ?headers ~error ok

    let get2 ?(post=false) ?headers ?(params=[]) ?msg ?error api service arg1 arg2 f =
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      if post then
        let url = forge2 api service arg1 arg2 []  in
        let content = encode_params service.s params in
        let content_type = Url.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth ?msg url ~content ~content_type ?headers ~error ok
      else
        let url = forge2 api service arg1 arg2 params in
        internal_get ~meth ?msg url ?headers ~error ok

    let post0 :
      type i.
      ?headers:(string * string) list ->
      ?params:(Param.t * param_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      ?error:error_handler ->
      input:i ->
      EzAPI.base_url ->
      (i, 'output, 'error, 'security) post_service0 ->
      (('output, 'error) Result.result -> unit) ->
      unit =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ?error ~input api service f ->
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      let input_encoding = Service.input service.s in
      let url = forge0 api service params in
      let content, content_type = match input_encoding with
        | Empty -> "", "application/json"
        | Raw [] -> input, "application/octet-stream"
        | Raw (h :: _) -> input, Mime.to_string h
        | Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            Url.encode_obj enc input, Url.content_type in
      internal_post ~meth ?msg url ~content ~content_type ?headers ~error ok

    let post1 :
      type i.
      ?headers:(string * string) list ->
      ?params:(Param.t * param_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      ?error:error_handler ->
      input:i ->
      EzAPI.base_url ->
      ('arg, i, 'output, 'error, 'security) post_service1 ->
      'arg ->
      (('output, 'error) Result.result -> unit) ->
      unit =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ?error ~input api service arg f ->
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      let input_encoding = Service.input service.s in
      let url = forge1 api service arg params in
      let content, content_type = match input_encoding with
        | Empty -> "", "application/json"
        | Raw [] -> input, "application/octet-stream"
        | Raw (h :: _) -> input, Mime.to_string h
        | Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            Url.encode_obj enc input, Url.content_type in
      internal_post ?msg ~meth url ~content ~content_type ?headers ~error ok

    let post2 :
      type i.
      ?headers:(string * string) list ->
      ?params:(Param.t * param_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      ?error:error_handler ->
      input:i ->
      EzAPI.base_url ->
      ('arg1, 'arg2, i, 'output, 'error, 'security) post_service2 ->
      'arg1 -> 'arg2 ->
      (('output, 'error) Result.result -> unit) ->
      unit =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ?error ~input api service arg1 arg2 f ->
      !before_hook ();
      let ok, error = handlers ?error service.s f in
      let meth = Service.meth service.s in
      let input_encoding = Service.input service.s in
      let url = forge2 api service arg1 arg2 params in
      let content, content_type = match input_encoding with
        | Empty -> "", "application/json"
        | Raw [] -> input, "application/octet-stream"
        | Raw (h :: _) -> input, Mime.to_string h
        | Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            Url.encode_obj enc input, Url.content_type in
      internal_post ?msg ~meth url ~content ~content_type ?headers ~error ok
  end

  include Raw

  module Legacy = struct

    let unresultize f = function
      | Ok res -> f res
      | Error _u -> assert false (* Security.unreachable u *)

    let get0 api service
        msg ?post ?headers ?error ?params f () =
      let msg = if msg = "" then None else Some msg in
      Raw.get0 api service ?msg ?post ?headers ?error ?params (unresultize f)

    let get1 api service
        msg ?post ?headers ?error ?params f arg =
      let msg = if msg = "" then None else Some msg in
      Raw.get1 api service ?msg ?post ?headers ?error ?params arg (unresultize f)

    let get2 api service
        msg ?post ?headers ?error ?params f arg1 arg2 =
      let msg = if msg = "" then None else Some msg in
      Raw.get2 api service ?msg ?post ?headers ?error ?params arg1 arg2 (unresultize f)

    let post0 api service
        msg ?headers ?error ?params ?url_encode ~input f =
      let msg = if msg = "" then None else Some msg in
      Raw.post0 api service ?msg ?headers ?error ?params ?url_encode ~input (unresultize f)

    let post1 api service
        msg ?headers ?error ?params ?url_encode ~input arg f =
      let msg = if msg = "" then None else Some msg in
      Raw.post1 api service ?msg ?headers ?error ?params ?url_encode ~input arg (unresultize f)

    let post2 api service
        msg ?headers ?error ?params ?url_encode ~input arg1 arg2 f =
      let msg = if msg = "" then None else Some msg in
      Raw.post2 api service ?msg ?headers ?error ?params ?url_encode ~input arg1 arg2 (unresultize f)

    let get ?meth msg url ?headers ?error f =
      let msg = if msg = "" then None else Some msg in
      get ?meth ?headers ?msg ?error url f

    let post ?meth ?content_type ?content msg url ?headers ?error f =
      let msg = if msg = "" then None else Some msg in
      post ?meth ?content_type ?content ?headers ?msg ?error url f

  end

end

module ANY : S = Make(struct
    let get ?meth ?headers ?msg url  f =
      !any_get ?meth ?msg url ?headers f
    let post ?meth ?content_type ?content ?headers ?msg url f =
      !any_post ?meth ?content_type ?content ?msg url ?headers f
  end)
