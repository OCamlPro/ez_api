open Lwt
open EzRequest

let get _msg url ?(headers=[]) f =
  let r () =
    let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) headers in
    Cohttp_lwt_unix.Client.get ~headers
                               (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    if code = 200 then begin
        Cohttp_lwt.Body.to_string body >>= fun body ->
        f (CodeOk body); Lwt.return_unit
      end else begin
        f (CodeError code);
        Lwt.return_unit
      end
  in
  Lwt.async r

let post ?(content_type = "application/json") ?(content="{}")
         _msg url ?(headers=[]) f =
  let r () =
    let body = Cohttp_lwt.Body.of_string content in
    let headers =
      Cohttp.Header.add_list (
          Cohttp.Header.init_with "Content-Type" content_type)
                             headers in
    Cohttp_lwt_unix.Client.post ~body ~headers
                                (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    if code = 200 then begin
        Cohttp_lwt.Body.to_string body >>= fun body ->
        f (CodeOk body); Lwt.return_unit
      end else begin
        f (CodeError code);
        Lwt.return_unit
      end
  in
  Lwt.async r

let init () =
  EzRequest.xhr_get := get;
  EzRequest.xhr_post := post;
  ()

let () = init ()

include EzRequest.ANY
