(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let timeout = ref (Some 30)
let set_timeout t = timeout := t

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let writer_callback a d =
  Buffer.add_string a d;
  String.length d

let init ?(meth="GET") ?(headers=[]) ~url l =
  let headers = match l with
    | [ _, `content _, Some ct ] -> ("content-type", ct) :: headers
    | _ -> headers in
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
  begin match l with
  | [ _, `content s, _ ] ->
    Curl.set_postfields c s;
    Curl.set_postfieldsize c (String.length s)
  | _ ->
    let l = List.map (fun (name, content, ct) ->
        let ct = match ct with None -> Curl.DEFAULT | Some s -> Curl.CONTENTTYPE s in
        match content with
        | `file s -> Curl.CURLFORM_FILE (name, s, ct)
        | `filecontent s -> Curl.CURLFORM_FILECONTENT (name, s, ct)
        | `content s -> Curl.CURLFORM_CONTENT (name, s, ct)) l in
    Curl.setopt c (Curl.CURLOPT_HTTPPOST l)
  end;
  r,c

let payload_to_string l =
  match l with
  | [ _, `content s, _ ] -> s
  | _ ->
    let content_to_string = function
      | `content s -> s
      | `filecontent s -> "filecontent:" ^ s
      | `file s -> "file:" ^ s in
    String.concat "\n" @@ List.map (fun (name, content, _) ->
      Format.sprintf "%s=%s" name (content_to_string content)) l
