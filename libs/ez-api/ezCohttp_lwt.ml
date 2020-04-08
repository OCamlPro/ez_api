open Lwt

include EzRequest_lwt.Make(struct

    let get ?(headers=[]) ?msg:_ url =
      let r () =
        let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) headers in
        Cohttp_lwt_unix.Client.get ~headers
          (Uri.of_string url) >>= fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >|= fun body ->
        if code >= 200 && code < 300 then Ok body
        else Error (code, Some body) in
      catch r (fun exn -> return (Error (-1, Some (Printexc.to_string exn))))

    let post ?(content_type = "application/json") ?(content="{}") ?(headers=[])
        ?msg:_ url =
      let r () =
        let body = Cohttp_lwt.Body.of_string content in
        let headers =
          Cohttp.Header.add_list (
            Cohttp.Header.init_with "Content-Type" content_type)
            headers in
        Cohttp_lwt_unix.Client.post ~body ~headers
          (Uri.of_string url) >>= fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        Cohttp_lwt.Body.to_string body >|= fun body ->
        if code >= 200 && code < 300 then Ok body
        else Error (code, Some body)
      in
      catch r (fun exn -> return (Error (-1, Some (Printexc.to_string exn))))

  end)
