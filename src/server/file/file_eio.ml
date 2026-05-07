let not_found path =
  EzDebug.printf "[%t] \027[0;31m 404 - /%s\027[0m" GMTime.pp_now path;
  { Answer.code = 404; body = ""; headers=[] }

let rec reply ~fs ?(meth=`GET) ?default root path =
  let path = String.concat "/" path in
  let file = Filename.concat root path in
  let content_type = EzAPI.Mime.content_type_of_file file in
  match meth with
  | `OPTIONS ->
    if Sys.file_exists file && not (Sys.is_directory file) then
      { Answer.code = 200; body = ""; headers=[Cors.allow_methods_name, "GET"] }
    else begin match path, default with
      | "", Some file -> reply ~fs ~meth root (String.split_on_char '/' file)
      | _ -> not_found path
    end
  | _ ->
    if Sys.file_exists file && not (Sys.is_directory file) then
      let body = Eio.Path.(load (fs / file)) in
      EzDebug.printf "[%t] \027[0;32m 200 - /%s\027[0m - %d" GMTime.pp_now path (String.length body);
      { Answer.code = 200; body; headers=["content-type", content_type] }
    else begin match path, default with
      | "", Some file -> reply ~fs ~meth root (String.split_on_char '/' file)
      | _ -> not_found path
    end
