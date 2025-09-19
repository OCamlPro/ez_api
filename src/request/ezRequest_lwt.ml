(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzAPI

module type S = EzReq_lwt_S.S
module type Interface = EzReq_lwt_S.Interface

let (>|=) = Lwt.(>|=)
let return = Lwt.return

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let any_get = ref (fun ?meth:_m ?headers:_ ?msg:_ _url ->
    return (Error (-2, Some "No http client loaded"))
  )
let any_post = ref (fun ?meth:_m ?content_type:_ ?content:_ ?headers:_ ?msg:_ _url ->
    return (Error (-2, Some "No http client loaded"))
  )


module Make(S : Interface) : S = struct

  let init () =
    any_get := S.get;
    any_post := S.post

  let add_user_agent ?headers () =
    match !Req.user_agent_header, headers with
    | None, _ -> headers
    | Some h, None -> Some [ h ]
    | Some h, Some l ->
      if List.exists (fun (k, _) -> String.lowercase_ascii k = "user-agent") l then Some l
      else Some (h :: l)

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?meth ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ Meth.to_string m) in
    let headers = add_user_agent ?headers () in
    S.get ?meth ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let internal_post ?meth ?content_type ?content ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ Meth.to_string m) in
    let headers = add_user_agent ?headers () in
    S.post ?meth ?content_type ?content ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let add_hook f =
    let old_hook = !before_hook in
    before_hook := (fun () -> old_hook (); f ())

  let add_reply_hook f =
    let old_hook = !request_reply_hook in
    request_reply_hook := (fun () -> old_hook (); f ())

  let get = internal_get
  let post = internal_post

  module Raw = struct
    type nonrec 'e api_error = 'e EzReq_lwt_S.api_error =
      | KnownError of { code : int ; error : 'e }
      | UnknownError of { code : int ; msg : string option }

    let decode_result io err_encodings = function
      | Error (code, None) -> Error (UnknownError { code ; msg = None })
      | Error (code, Some msg) ->
        (match err_encodings ~code with
         | None -> Error (UnknownError { code ; msg = Some msg })
         | Some encoding ->
           try Error (
               KnownError { code ; error = EzEncoding.destruct encoding msg })
           with _ -> Error (UnknownError { code ; msg = Some msg })
        )
      | Ok res ->
        match IO.from_string io (fun x -> x) res with
        | Ok s -> Ok s
        | Error (`destruct_exn exn) ->
          let msg = match exn with
            | Json_encoding.Cannot_destruct _ ->
              Json_encoding.print_error Format.str_formatter exn;
              Format.flush_str_formatter ()
            | _ -> Printexc.to_string exn in
          Error (UnknownError {
            code = -3;
            msg = Some msg })

    let handle_result service res =
      let err_encodings = Service.error service.s in
      let encoding = Service.output service.s in
      decode_result encoding err_encodings res

    let request :
      type i.
      ?headers:(string * string) list ->
      ?params:(Param.t * param_value) list ->
      ?msg:string ->
      ?post:bool ->
      ?url_encode:bool ->
      input:i ->
      EzAPI.base_url ->
      ('arg, i, 'output, 'error, 'security) service ->
      'arg ->
      ('output, 'error api_error) result Lwt.t =
      fun ?headers ?(params=[]) ?msg ?(post=false) ?(url_encode=false) ~input api service arg ->
      !before_hook ();
      let meth = Service.meth service.s in
      let input_encoding = Service.input service.s in
      let url = forge api service arg params in
      begin match input_encoding with
        | Empty ->
          if post then
            let url = forge api service arg [] in
            let content, content_type = encode_params service.s params, Mime.(to_string url_encoded) in
            internal_post ?msg ~meth ~content ~content_type ?headers url
          else
            internal_get ~meth ?msg ?headers url
        | Raw [] ->
          let content, content_type = input, "application/octet-stream" in
          internal_post ?msg ~meth ~content ~content_type ?headers url
        | Raw [ h ] when h = Mime.url_encoded && input = "" ->
          let url = forge api service arg [] in
          let content, content_type = encode_params service.s params, Mime.to_string h in
          internal_post ?msg ~meth ~content ~content_type ?headers url
        | Raw (h :: _) ->
          let content, content_type = input, Mime.to_string h in
          internal_post ?msg ~meth ~content ~content_type ?headers url
        | Json enc ->
          let content, content_type =
            if url_encode then Url.encode_obj enc input, Mime.(to_string url_encoded)
            else EzEncoding.construct enc input, Mime.(to_string json) in
          internal_post ?msg ~meth ~content ~content_type ?headers url
      end >|= handle_result service

    let get0 ?post ?headers ?params ?msg api service =
      request ?headers ?params ?msg ?post ~input:() api service Req.dummy

    let get1 ?post ?headers ?params ?msg api service arg =
      request ?headers ?params ?msg ?post ~input:() api service (Req.dummy, arg)

    let get2 ?post ?headers ?params ?msg api service arg1 arg2 =
      request ?headers ?params ?msg ?post ~input:() api service ((Req.dummy, arg1), arg2)

    let post0 ?headers ?params ?msg ?url_encode ~input api service =
      request ?headers ?params ?msg ?url_encode ~input api service Req.dummy

    let post1 ?headers ?params ?msg ?url_encode ~input api service arg =
      request ?headers ?params ?msg ?url_encode ~input api service (Req.dummy, arg)

    let post2 ?headers ?params ?msg ?url_encode ~input api service arg1 arg2 =
      request ?headers ?params ?msg ?url_encode ~input api service ((Req.dummy, arg1), arg2)

    let handle_error kn = function
      | KnownError {code; error} -> code, kn error
      | UnknownError {code; msg} -> code, msg

    let string_of_error kn = function
      | KnownError {code; error} ->
        let content = match kn error with None -> "" | Some s -> ": " ^ s in
        Printf.sprintf "Error %d%s" code content
      | UnknownError {code; msg} ->
        let content = match msg with None -> "" | Some s -> ": " ^ s in
        Printf.sprintf "Unknown Error %d%s" code content

    let pp_error ?error pp = function
      | UnknownError {code; msg} ->
        Format.fprintf pp "Error %d%s" code
          (Option.fold ~none:"" ~some:(fun s -> ": " ^ s) msg)
      | KnownError {code; error=e} ->
        match error with
        | None -> Format.fprintf pp "Error %d" code
        | Some ppe -> Format.fprintf pp "Error %d: %a" code ppe e

    let wrap p = Lwt.map (Result.map_error (function
        | UnknownError {code; msg} -> code, `unknown msg
        | KnownError {code; error} -> code, `known error)) p
  end

  include Raw

  module Legacy = struct

    type _ api_error = int * string option

    type ('output, 'error, 'security) service0 =
      ('output) Legacy.service0
      constraint 'security = [< Security.scheme ]

    type ('arg, 'output, 'error, 'security) service1 =
      ('arg, 'output) Legacy.service1
      constraint 'security = [< Security.scheme ]

    type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
      ('arg1, 'arg2, 'output) Legacy.service2
      constraint 'security = [< Security.scheme ]

    type ('input, 'output, 'error, 'security) post_service0 =
      ('input, 'output) Legacy.post_service0
      constraint 'security = [< Security.scheme ]

    type ('arg, 'input, 'output, 'error, 'security) post_service1 =
      ('arg, 'input, 'output) Legacy.post_service1
      constraint 'security = [< Security.scheme ]

    type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 =
      ('arg1, 'arg2, 'input, 'output) Legacy.post_service2
      constraint 'security = [< Security.scheme ]

    type ('arg, 'input, 'output, 'error, 'security) service =
      (unit, 'arg, 'input, 'output) Legacy.service
      constraint 'security = [< Security.scheme ]

    open EzAPI.Legacy

    let unresultize = function
      | Ok res -> Ok res
      | Error UnknownError { code ; msg } -> Error (code, msg)
      | Error KnownError _ -> assert false (* Security.unreachable error *)

    let get0 ?post ?headers ?params ?msg
        api (service: 'output EzAPI.Legacy.service0) =
      get0 ?post ?headers ?params ?msg api service
      >|= unresultize

    let get1 ?post ?headers ?params ?msg
        api (service : ('arg,'output) service1) (arg : 'arg) =
      get1 ?post ?headers ?params ?msg api service arg
      >|= unresultize

    let get2 ?post ?headers ?params ?msg
        api (service : ('arg1, 'arg2, 'output) service2) (arg1 : 'arg1) (arg2 : 'arg2) =
      get2 ?post ?headers ?params ?msg api service arg1 arg2
      >|= unresultize

    let post0 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('input,'output) post_service0) =
      post0 ?headers ?params ?msg ?url_encode ~input api service
      >|= unresultize

    let post1 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('arg, 'input,'output) post_service1) (arg : 'arg) =
      post1 ?headers ?params ?msg ?url_encode ~input api service arg
      >|= unresultize

    let post2 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('arg1, 'arg2, 'input, 'output) post_service2)
        (arg1 : 'arg1) (arg2 : 'arg2) =
      post2 ?headers ?params ?msg ?url_encode ~input api service arg1 arg2
      >|= unresultize

    let request ?headers ?params ?msg ?post ?url_encode ~(input: 'input)
        api (service : (unit, 'arg, 'input, 'output) service)
        (arg : 'arg) =
      request ?headers ?params ?msg ?post ?url_encode ~input api service arg
      >|= unresultize

    let handle_error _ x = x

    let string_of_error _ (code, content) =
        let content = match content with None -> "" | Some s -> ": " ^ s in
        Printf.sprintf "Error %d%s" code content

    let pp_error ?error:_ pp (code, content) =
      Format.fprintf pp "Error %d%s" code
        (Option.fold ~none:"" ~some:(fun s -> ": " ^ s) content)
    let wrap p = Lwt.map (Result.map_error (fun (code, content) -> code, `unknown content)) p

  end

end

module ANY = Make(struct
    let get ?meth ?headers ?msg url = !any_get ?meth ?headers ?msg url
    let post ?meth ?content_type ?content ?headers ?msg url =
      !any_post ?meth ?content_type ?content ?headers ?msg url
  end)
