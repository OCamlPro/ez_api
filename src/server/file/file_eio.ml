let not_found path =
  let cstart, cend = EzAPI.apply_ansi_color 31 in
  Log_eio.printf "[%t]%s 404 /%s%s@." GMTime.pp_now cstart path cend;
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
        let cstart, cend = EzAPI.apply_ansi_color 32 in
        Log_eio.printf "[%t]%s 200 /%s%s - %a@." GMTime.pp_now cstart ori cend Log.pp_content_length (String.length body);
        { Answer.code = 200; body; headers=["content-type", content_type] }
      else begin match ori, default with
        | "", Some file -> aux file
        | _ -> not_found ori
      end in
  aux ?default ori
