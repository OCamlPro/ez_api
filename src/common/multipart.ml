type 'content form_data = {
  fo_name: string;
  fo_content: 'content;
  fo_filename: string option;
  fo_headers: string list Req.StringMap.t;
}

let make ?(headers=[]) ?filename ~name content = {
  fo_name=name; fo_content=content; fo_filename=filename;
  fo_headers=List.fold_left (fun acc (k, v) -> Req.StringMap.add k v acc) Req.StringMap.empty headers;
}

let split sep s =
  let n, nsep = String.length s, String.length sep in
  let r, j = ref [], ref n in
  for i = n - nsep downto 0 do
    if String.sub s i nsep = sep then (
      r := String.sub s (i + nsep) (!j - i - nsep) :: !r;
      j := i
    )
  done;
  String.sub s 0 !j :: !r

let process_header ?(debug=false) map s = match String.split_on_char ':' s with
  | [ k; v ] ->
    let k, v = String.lowercase_ascii (String.trim k), String.trim v in
    Req.StringMap.add k (String.split_on_char ',' v) map
  | _ ->
    if debug then Format.eprintf "malformed header: %s@." s;
    map

let make_form_data ?debug ?(index=0) ~headers contents =
  let headers = List.fold_left (process_header ?debug) Req.StringMap.empty headers in
  let unquote s =
    let n = String.length s in
    if n > 1 && String.get s 0 = '"' && String.get s (n-1) = '"' then String.sub s 1 (n-2)
    else s in
  let fo_content = String.concat "/r/n" contents in
  let default = Format.sprintf "part%d" index in
  let fo_name, fo_filename, fo_headers = match Req.StringMap.find_opt "content-disposition" headers with
    | Some [ cd ] ->
      let _, params = Req.header_params ?debug cd in
      Option.value ~default (List.find_map (fun (k, v) -> if k="name" then Some (unquote v) else None) params),
      List.find_map (fun (k, v) -> if k="filename" then Some (unquote v) else None) params,
      Req.StringMap.remove "content-disposition" headers
    | _ -> default, None, headers in
  { fo_name; fo_filename; fo_headers; fo_content }

let get_boundary_from_content_type ?debug ct =
  let value, params = Req.header_params ?debug ct in
  if value <> "multipart/form-data" then Error "content is not multipart/form-data"
  else
    match List.find_map (fun (k, v) -> if k = "boundary" then Some v else None) params with
    | None -> Error "boundary parameter not found"
    | Some b -> Ok b

let get_boundary ?debug headers =
  match Req.StringMap.find_opt "content-type" headers with
  | Some [ ct ] -> get_boundary_from_content_type ?debug ct
  | _ -> Error "content-type header not found"

let (let$) = Result.bind

let parse_content ?debug ~boundary s =
  let l = split "\r\n" s in
  let start = "--" ^ boundary in
  let end_ = start ^ "--" in
  let rec aux acc l = match acc, l with
    | `start, h :: tl when h = start -> aux (`header (1, [], [])) tl
    | `start, h :: _ when h = end_ -> Ok []
    | `header (index, headers, acc), "" :: tl -> aux (`content (index, List.rev headers, [], acc)) tl
    | `content (index, headers, contents, acc), h :: tl when h = start ->
      let data = make_form_data ?debug ~index ~headers (List.rev contents) in
      aux (`header (index+1, [], data :: acc)) tl
    | `content (index, headers, contents, acc), h :: _ when h = end_ ->
      let data = make_form_data ?debug ~index ~headers (List.rev contents) in
      Ok (List.rev @@ data :: acc)
    | `header (index, headers, acc), h :: tl -> aux (`header (index, h :: headers, acc)) tl
    | `content (index, headers, contents, acc), h :: tl -> aux (`content (index, headers, h :: contents, acc)) tl
    | _ -> Error "wrong multipart format" in
  aux `start l

let parse ?debug ~headers s =
  let$ boundary = get_boundary ?debug headers in
  parse_content ?debug ~boundary s

let make_boundary ?(length=16) () =
  "ezAPI" ^ String.init length (fun _ ->
      let i = Random.int 62 in
      let code = if i < 10 then i+48 else if i < 36 then i+55 else i+61 in
      Char.chr code)

let produce_form_data ~boundary b data =
  Buffer.add_string b ("--" ^ boundary ^ "\r\n");
  Buffer.add_string b ("content-disposition: form-data; name=\"" ^ data.fo_name ^ "\"");
  Option.iter (fun filename -> Buffer.add_string b ("; filename=\"" ^ filename ^ "\"")) data.fo_filename;
  Buffer.add_string b "\r\n";
  Req.StringMap.iter (fun k lv ->
      Buffer.add_string b (k ^ ":");
      let () = match lv with
        | [] -> ()
        | h :: tl ->
          Buffer.add_string b h;
          List.iter (fun v -> Buffer.add_string b (", " ^ v)) tl in
      Buffer.add_string b "\r\n") data.fo_headers;
  Buffer.add_string b "\r\n";
  Buffer.add_string b data.fo_content;
  Buffer.add_string b "\r\n"

let wrap_produce ?boundary f =
  let boundary = match boundary with Some b -> b | None -> make_boundary () in
  let b = Buffer.create 500 in
  f ~boundary b;
  Buffer.add_string b ("--" ^ boundary ^ "--\r\n");
  Buffer.contents b

let produce ?boundary l =
  wrap_produce ?boundary @@ fun ~boundary b ->
  List.iter (produce_form_data ~boundary b) l

let content_type boundary = "multipart/form-data; boundary=" ^ boundary
