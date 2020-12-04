
let cut_at s c =
  try
    let pos = String.index s c in
    let len = String.length s in
    String.sub s 0 pos,
    String.sub s (pos+1) (len - pos - 1)
  with _ -> s, ""



(* encode using x-www-form-urlencoded form *)
let encode s =
  let pos = ref 0 in
  let len = String.length s in
  let res = Bytes.create (3*len) in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x) in
  for i=0 to len-1 do
    match String.get s i with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' | '*' | '_' ->
        Bytes.set res !pos (String.get s i); incr pos
(*    | ' ' -> res.[!pos] <- '+'; incr pos *)
    | c ->
        Bytes.set res !pos '%';
        Bytes.set res (!pos+1) @@ hexa_digit (Char.code c / 16);
        Bytes.set res (!pos+2) @@ hexa_digit (Char.code c mod 16);
        pos := !pos + 3
  done;
  Bytes.sub_string res 0 !pos

(* decode using x-www-form-urlencoded form *)

let digit_hexa x =
  match x with
  | 'a' .. 'f' -> (Char.code x) + 10 - (Char.code 'a')
  | 'A' .. 'F' -> (Char.code x) + 10 - (Char.code 'A')
  | '0' .. '9' -> (Char.code x) - (Char.code '0')
  | _ -> failwith "Not an hexa number (encode.ml)"

let decode s =
  let len = String.length s in
  let r = Buffer.create len in
  let rec iter i =
    if i < len then
      match s.[i] with
      | '+' -> Buffer.add_char r  ' '; iter (i+1)
      | '%' ->
          let n =
            try
              let fst = digit_hexa s.[i+1] in
              let snd = digit_hexa s.[i+2] in
              Buffer.add_char r (char_of_int (fst*16 + snd));
              3
            with _ ->
                Buffer.add_char r '%';
                1
          in
          iter (i+n)

      | c -> Buffer.add_char r c; iter (i+1)
  in
  iter 0;
  Buffer.contents r

let encode_args ?(url=false) l =
  String.concat "&" (List.map (fun (name, arg) ->
      Printf.sprintf "%s=%s" name
        (String.concat ","
           (if url then List.map encode arg else arg))) l)

let decode_args ?(url=false) s =
  let args = String.split_on_char '&' s in
  List.map (fun s ->
      let s, v = cut_at s '=' in
      let v = String.split_on_char ',' v in
      let s = decode s in
      let v = if url then List.map decode v else v in
      s, v
    ) args

let content_type = "application/x-www-form-urlencoded"

let encode_obj ?(url=false) enc x =
  let rec aux ?prefix = function
    | `Null -> None
    | `String s -> let s = if url then encode s else s in
      Some (match prefix with None -> s | Some p -> p ^ "=" ^ s)
    | `Float f -> let s = string_of_float f in
      Some (match prefix with None -> s | Some p -> p ^ "=" ^ s)
    | `Bool b -> let s = string_of_bool b in
      Some (match prefix with None -> s | Some p -> p ^ "=" ^ s)
    | `A l ->
      if l = [] then None else
        Some (String.concat "&" @@ List.filter_map (aux ?prefix) l)
    | `O l ->
      if l = [] then None else
        Some (String.concat "&" @@ List.filter_map (fun (k, v) ->
            let prefix = match prefix with None -> k | Some p -> p ^ "[" ^ k ^ "]" in
            aux ~prefix v) l) in
  match aux (Json_encoding.construct enc x) with
  | None -> ""
  | Some s -> s
