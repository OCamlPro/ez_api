open Lwt
open EzRequest

let meth_of_str s =
  let open Cohttp.Code in match s with
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "CONNECT" -> `CONNECT
  | "PATCH" -> `PATCH
  | "TRACE" -> `TRACE
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | s -> `Other s

include EzRequest.Make(struct

    let xhr_get ?(meth="GET") _msg url ?(headers=[]) f =
      let r () =
        let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) headers in
        Cohttp_lwt_unix.Client.call ~headers (meth_of_str meth)
          (Uri.of_string url) >>= fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        if code = 200 then begin
          f (CodeOk body);
        end else begin
          f (CodeError (code, Some body));
        end;
        Lwt.return_unit
      in
      Lwt.async
        (fun () ->
           Lwt.catch r
             (fun exn ->
                f (CodeError(-1, Some (Printexc.to_string exn)));
                Lwt.return_unit
             ))

    let xhr_post ?(meth="POST") ?(content_type = "application/json") ?(content="{}")
        _msg url ?(headers=[]) f =
      let r () =
        let body = Cohttp_lwt.Body.of_string content in
        let headers =
          Cohttp.Header.add_list (
            Cohttp.Header.init_with "Content-Type" content_type)
            headers in
        Cohttp_lwt_unix.Client.call ~body ~headers (meth_of_str meth)
          (Uri.of_string url) >>= fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        if code = 200 then begin
          f (CodeOk body);
        end else begin
          f (CodeError (code, Some body));
        end;
        Lwt.return_unit
      in
      Lwt.async r

  end)
