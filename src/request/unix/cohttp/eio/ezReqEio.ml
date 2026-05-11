
let make ~(meth: EzAPI.Meth.t) ?(headers=[]) ?msg ?content_type ?content ~net ~sw url =
  Verbose.request ?msg ~meth:(EzAPI.Meth.to_string meth) ?content url;
  let body = Option.map Cohttp_eio.Body.of_string content in
  let headers = Cohttp.Header.of_list headers in
  let headers = match content_type with None -> headers | Some ct -> Cohttp.Header.add headers "content-type" ct in
  let client = Cohttp_eio.Client.make ~https:Cohttp_eio_tls.https net in
  let r, body = Cohttp_eio.Client.call ?body ~headers ~sw client (meth :> Cohttp.Code.meth) (Uri.of_string url) in
  let code = Cohttp.Code.code_of_status @@ Cohttp.Response.status r in
  let headers = Cohttp.Header.to_list @@ Cohttp.Response.headers r in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
  Verbose.response ?msg ~code ~content:body url;
  (code, body, headers)

module Interface = struct

  let get ?(meth=`GET) ?headers ?msg ~net ~sw url =
    make ~meth ?headers ?msg ~net ~sw url

  let post ?(meth=`POST) ?headers ?msg ?content_type ?content ~net ~sw url =
    make ~meth ?headers ?content_type ?content ?msg ~net ~sw url

end

include EzRequestEio.Make(Interface)
