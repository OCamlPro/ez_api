let timeout = ref (Some 30)
let set_timeout t = timeout := t

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let writer_callback a d =
  Buffer.add_string a d;
  String.length d

let init ?(meth="GET") ?content ?content_type ?(headers=[]) url =
  let headers = match content_type with
    | None -> headers
    | Some ct -> ("content-type", ct) :: headers in
  let r = Buffer.create 16384
  and c = Curl.init () in
  (match !timeout with None -> () | Some t -> Curl.set_timeout c t);
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  Curl.set_writefunction c (writer_callback r);
  Curl.set_verbose c (!Verbose.v land 4 <> 0);
  Curl.set_customrequest c meth;
  Curl.set_followlocation c true;
  Curl.set_url c url;
  Curl.set_httpheader c (
    List.map (fun (name, value) -> Format.sprintf "%s: %s" name value) headers);
  (match content with
   | Some content ->
     Curl.set_postfields c content;
     Curl.set_postfieldsize c (String.length content)
   | _ -> ());
  r,c
