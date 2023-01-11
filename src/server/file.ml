(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module FileString = struct
  let size = 16_000
  let s = Bytes.make size '\000'
  let b = Buffer.create size

  let read_file ic =
    Buffer.clear b;
    let rec iter ic b s =
      let nread = input ic s 0 size in
      if nread > 0 then begin
        Buffer.add_subbytes b s 0 nread;
        iter ic b s
      end
    in
    iter ic b s;
    Buffer.contents b

  let read_file filename =
    let ic = open_in_bin filename in
    try
      let s = read_file ic in
      close_in ic;
      s
    with e ->
      close_in ic;
      raise e
end

let normalize_path path =
  let rec normalize_path path revpath =
    match path, revpath with
    | [],_ -> List.rev revpath
    | ("" | ".") :: path, _ -> normalize_path path revpath
    | ".." :: path, _ :: revpath -> normalize_path path revpath
    | ".." :: path, [] -> normalize_path path []
    | dir :: path, revpath -> normalize_path path (dir :: revpath)
  in
  normalize_path path []

let rec reply ?(meth=`GET) ?default root path =
  let path = normalize_path path in
  let file = Filename.concat root (String.concat "/" path) in
  let content_type = EzAPI.Mime.content_type_of_file file in
  match meth with
  | `OPTIONS ->
    if Sys.file_exists file then
      Lwt.return {Answer.code = 200; body = ""; headers = ["access-control-allow-methods", "GET"]}
    else begin match default with
      | None ->
        Lwt.return {Answer.code = 404; body = ""; headers = []}
      | Some file ->
        reply ~meth root (String.split_on_char '/' file)
    end
  | _ ->
    let body = FileString.read_file file in
    EzDebug.printf "Returning file %S of length %d" file
      (String.length body);
    Lwt.return {Answer.code = 200; body; headers = ["content-type", content_type]}
