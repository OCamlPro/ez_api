type str_or_star = [ `star | `str of string ]

type t = {
  typ : str_or_star;
  subtyp : str_or_star;
  param : (string * string) option
}

let parse m =
  match String.index_opt m '/' with
  | None -> None
  | Some i ->
    let typ = String.sub m 0 i in
    let typ = if typ = "*" then `star else `str typ in
    match String.index_from_opt m (i+1) ';' with
    | None ->
      let subtyp = String.sub m (i+1) (String.length m - i - 1) in
      let subtyp = if subtyp = "*" then `star else `str subtyp in
      Some {typ; subtyp; param = None}
    | Some j ->
      let subtyp = String.sub m (i+1) (j - i - 1) in
      let subtyp = if subtyp = "*" then `star else `str subtyp in
      match String.index_from_opt m (j+1) '=' with
      | None -> Some {typ; subtyp; param = None}
      | Some k ->
        let key = String.sub m (j+1) (k - j - 1) in
        let v = String.sub m (k+1) (String.length m - k - 1) in
        Some {typ; subtyp; param = Some (key, v)}

let to_string m =
  let typ = match m.typ with `str s -> s | `star -> "*" in
  let subtyp = match m.subtyp with `str s -> s | `star -> "*" in
  let param = match m.param with None -> "" | Some (k, v) -> ";" ^ k ^ "=" ^ v in
  typ ^ "/" ^ subtyp ^ param

let allowed l c =
  if l = [] then true
  else match c with
    | None -> List.exists (fun {typ; subtyp; _} -> typ = `star && subtyp = `star) l
    | Some s ->
      match parse s with
      | None -> false
      | Some {typ; subtyp; _} ->
        let rec aux = function
          | [] -> false
          | h :: t ->
            ((h.typ = `star || h.typ = typ) && (h.subtyp = `star || h.subtyp = subtyp)) ||
            aux t in
        aux l

let json = {typ = `str "application"; subtyp = `str "json"; param = None }
let multipart = {typ = `str "multipart"; subtyp = `str "form-data"; param = None }


let content_type_of_file file =
  let exts =
    List.rev (String.split_on_char '.' (String.lowercase_ascii (Filename.basename file))) in
  match exts with
  | "js" :: _ -> "text/javascript"
  | "txt" :: _ -> "text/plain"
  | "pdf" :: _ -> "application/pdf"
  | "json" :: _ -> "application/json"
  | "xml" :: _ -> "application/xml"
  | "zip" :: _ -> "application/zip"
  | ("html" | "htm") :: _ -> "text/html"
  | "map" :: "css" :: _
  | "css" :: _ -> "text/css"
  | "png" :: _ -> "image/png"
  | "jpg" :: _ | "jpeg" :: _ | "jfif" :: _ | "pjpeg" :: _ | "pjp" :: _ -> "image/jpeg"
  | "gif" :: _ -> "image/gif"
  | "svg" :: _ -> "image/svg+xml"
  | "webp" :: _ -> "image/webp"
  | "avif" :: _ -> "image/avif"
  | "apng" :: _ -> "image/apng"
  | _ -> "application/octet-stream"
