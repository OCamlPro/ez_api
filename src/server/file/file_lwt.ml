open Lwt.Infix

let read_file =
  let size = 16_000 in
  let s = Bytes.make size '\000' in
  let b = Buffer.create size in
  fun filename ->
    Buffer.clear b;
    Lwt.catch
      (fun () ->
         Lwt_io.open_file ~mode:Lwt_io.Input ~flags:[Unix.O_RDONLY] filename >|= Option.some)
      (fun _ -> Lwt.return_none) >>= function
    | None -> Lwt.return_none
    | Some ic ->
      let rec iter ic b s =
        Lwt.bind (Lwt_io.read_into ic s 0 size) @@ fun nread ->
        if nread > 0 then (
          Buffer.add_subbytes b s 0 nread;
          iter ic b s)
        else Lwt.return (Buffer.contents b) in
      Lwt.catch
        (fun () ->
           Lwt.bind (iter ic b s) @@ fun s ->
           Lwt.bind (Lwt_io.close ic) @@ fun () ->
           Lwt.return_some s)
        (fun exn ->
           Lwt.bind (Lwt_io.close ic) @@ fun () ->
           Lwt.reraise exn)

let rec reply ?(meth=`GET) ?default root path =
  let file = Filename.concat root (String.concat "/" path) in
  let content_type = EzAPI.Mime.content_type_of_file file in
  match meth with
  | `OPTIONS ->
    if Sys.file_exists file then
      Lwt.return { Answer.code = 200; body = ""; headers=[Cors.allow_methods_name, "GET"] }
    else begin match default with
      | None -> Lwt.return { Answer.code = 404; body = ""; headers=[] }
      | Some file -> reply ~meth root (String.split_on_char '/' file)
    end
  | _ ->
    if Sys.file_exists file then
      Lwt.bind (read_file file) @@ function
      | None -> Lwt.return { Answer.code = 404; body = ""; headers=[] }
      | Some body ->
        EzDebug.printf "Returning file %S of length %d" file (String.length body);
        Lwt.return { Answer.code = 200; body; headers=["content-type", content_type] }
    else begin match default with
      | None -> Lwt.return { Answer.code = 404; body = ""; headers=[] }
      | Some file -> reply ~meth root (String.split_on_char '/' file)
    end
