let log ?(meth="GET") url = function
  | None -> if !Verbose.v <> 0 then Format.printf "[ez_api] %s %s@." meth url
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let make ~(meth: EzAPI.Meth.t) ?(headers=[]) ?msg ?content_type ?content ~net ~sw url =
  log ~meth:(EzAPI.Meth.to_string meth) url msg;
  if !Verbose.v land 2 <> 0 then (
    match content with
    | Some s when s <> "" -> Format.printf "[ez_api] sent:\n%s@." s
    | _ -> ());
  let body = Option.map Cohttp_eio.Body.of_string content in
  let headers = Cohttp.Header.of_list headers in
  let headers = match content_type with None -> headers | Some ct -> Cohttp.Header.add headers "content-type" ct in
  let client = Cohttp_eio.Client.make ~https:Cohttp_eio_tls.https net in
  let r, body = Cohttp_eio.Client.call ?body ~headers ~sw client (meth :> Cohttp.Code.meth) (Uri.of_string url) in
  let code = Cohttp.Code.code_of_status @@ Cohttp.Response.status r in
  let headers = Cohttp.Header.to_list @@ Cohttp.Response.headers r in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
  log ~meth:("RECV " ^ string_of_int code) url msg;
  if !Verbose.v land 1 <> 0 && body <> "" then Format.printf "[ez_api] received:\n%s@." body;
  (code, body, headers)

module Interface = struct

  let get ?(meth=`GET) ?headers ?msg ~net ~sw url =
    make ~meth ?headers ?msg ~net ~sw url

  let post ?(meth=`POST) ?headers ?msg ?content_type ?content ~net ~sw url =
    make ~meth ?headers ?content_type ?content ?msg ~net ~sw url

end

include EzRequestEio.Make(Interface)
