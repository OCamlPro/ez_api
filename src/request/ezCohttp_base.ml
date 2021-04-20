open Lwt.Infix

let meth_of_str = function
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

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Printf.printf "[>%s %s %s ]\n%!" msg meth url

module Make(Client:Cohttp_lwt.S.Client) = struct
  let get ?(meth="GET") ?(headers=[]) ?msg url =
    log ~meth url msg;
    let r () =
      let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) headers in
      Client.call ~headers (meth_of_str meth)
        (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      Cohttp_lwt.Body.to_string body >|= fun body ->
      log ~meth:("RECV " ^ string_of_int code) url msg;
      if code >= 200 && code < 300 then Ok body
      else Error (code, Some body) in
    Lwt.catch r (fun exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))

  let post ?(meth="POST") ?(content_type = "application/json") ?(content="{}") ?(headers=[])
      ?msg url =
    log ~meth url msg;
    let r () =
      let body = Cohttp_lwt.Body.of_string content in
      let headers =
        Cohttp.Header.add_list (
          Cohttp.Header.init_with "Content-Type" content_type)
          headers in
      Client.call ~body ~headers (meth_of_str meth)
        (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      Cohttp_lwt.Body.to_string body >|= fun body ->
      log ~meth:("RECV " ^ string_of_int code) url msg;
      if code >= 200 && code < 300 then Ok body
      else Error (code, Some body)
    in
    Lwt.catch r (fun exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))
end
