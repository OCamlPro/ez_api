open EzAPI
open EzRequest_common

module type S = EzReqEioS.S
module type Interface = EzReqEioS.Interface

module Make(S : Interface) : S = struct

  let get ?meth ?headers ?msg ~net ~sw (URL url) =
    EzAPI.warnings (fun s -> Printf.ksprintf EzDebug.log "EzRequest.warning: %s" s);
    let headers = add_user_agent ?headers () in
    S.get ?meth ?headers ?msg ~net ~sw url

  let post ?meth ?headers ?msg ?content_type ?content ~net ~sw (URL url) =
    EzAPI.warnings (fun s -> Printf.ksprintf EzDebug.log "EzRequest.warning: %s" s);
    let headers = add_user_agent ?headers () in
    S.post ?meth ?content_type ?content ?headers ?msg ~net ~sw url

  let decode_result io err_encodings (code, content, _headers) =
    if code < 200 || code >= 300 then
      if content = "" then Error (code, `unknown None)
      else match err_encodings ~code with
        | None -> Error (code, `unknown (Some content))
        | Some encoding ->
          try Error (code, `known (EzEncoding.destruct encoding content))
          with _ -> Error (code, `unknown (Some content))
    else match IO.from_string io Fun.id content with
      | Ok s -> Ok s
      | Error (`destruct_exn exn) ->
        let msg = Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn in
        Error (-3, `unknown (Some msg))

  let handle_result service res =
    let err_encodings = Service.error service.s in
    let encoding = Service.output service.s in
    decode_result encoding err_encodings res

  let request ?headers ?params ?msg ?post:p ?url_encode ~net ~sw ~input api service arg =
    let meth = Service.meth service.s in
    let r = handle_input ?params ?post:p ?url_encode ~input api service arg
        (get ~meth ?headers ?msg ~net ~sw, post ~meth ?headers ?msg ~net ~sw) in
    handle_result service r

  let get0 ?headers ?params ?msg ?post ~net ~sw api service =
    request ?headers ?params ?msg ?post ~input:() ~net ~sw api service Req.dummy

  let get1 ?headers ?params ?msg ?post ~net ~sw api service arg =
    request ?headers ?params ?msg ?post ~input:() ~net ~sw api service (Req.dummy, arg)

  let get2 ?headers ?params ?msg ?post ~net ~sw api service arg1 arg2 =
    request ?headers ?params ?msg ?post ~input:() ~net ~sw api service ((Req.dummy, arg1), arg2)

  let post0 ?headers ?params ?msg ?url_encode ~net ~sw ~input api service =
    request ?headers ?params ?msg ?url_encode ~net ~sw ~input api service Req.dummy

  let post1 ?headers ?params ?msg ?url_encode ~net ~sw ~input api service arg =
    request ?headers ?params ?msg ?url_encode ~net ~sw ~input api service (Req.dummy, arg)

  let post2 ?headers ?params ?msg ?url_encode ~net ~sw ~input api service arg1 arg2 =
    request ?headers ?params ?msg ?url_encode ~net ~sw ~input api service ((Req.dummy, arg1), arg2)


end
