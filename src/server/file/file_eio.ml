let not_found path =
  EzDebug.printf "[%t]\027[0;31m 404 /%s\027[0m" GMTime.pp_now path;
  { Answer.code = 404; body = ""; headers=[] }

let reply ~fs ?(meth=`GET) ?default root path =
  let ori = String.concat "/" path in
  let rec aux ?default path =
    let file = Filename.concat root path in
    let content_type = EzAPI.Mime.content_type_of_file file in
    match meth with
    | `OPTIONS ->
      if Sys.file_exists file && not (Sys.is_directory file) then
        { Answer.code = 200; body = ""; headers=[Cors.allow_methods_name, "GET"] }
      else begin match ori, default with
        | "", Some file -> aux file
        | _ -> not_found ori
      end
    | _ ->
      if Sys.file_exists file && not (Sys.is_directory file) then
        let body = Eio.Path.(load (fs / file)) in
        EzDebug.printf "[%t]\027[0;32m 200 /%s\027[0m - %a" GMTime.pp_now ori Log.pp_content_length (String.length body);
        { Answer.code = 200; body; headers=["content-type", content_type] }
      else begin match ori, default with
        | "", Some file -> aux file
        | _ -> not_found ori
      end in
  aux ?default ori
