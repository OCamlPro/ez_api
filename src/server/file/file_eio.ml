let rec reply ~fs ?(meth=`GET) ?default root path =
  let file = Filename.concat root (String.concat "/" path) in
  let content_type = EzAPI.Mime.content_type_of_file file in
  match meth with
  | `OPTIONS ->
    if Sys.file_exists file then
      { Answer.code = 200; body = ""; headers=[Cors.allow_methods_name, "GET"] }
    else begin match default with
      | None -> { Answer.code = 404; body = ""; headers=[] }
      | Some file -> reply ~fs ~meth root (String.split_on_char '/' file)
    end
  | _ ->
    if Sys.file_exists file then
      let body = Eio.Path.(load (fs / file)) in
      EzDebug.printf "Returning file %S of length %d" file (String.length body);
      { Answer.code = 200; body; headers=["content-type", content_type] }
    else begin match default with
      | None -> { Answer.code = 404; body = ""; headers=[] }
      | Some file -> reply ~fs ~meth root (String.split_on_char '/' file)
    end
