(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzAPIServerUtils

(* RFC 2965 has
    cookie          =  "Cookie:" cookie-version 1*((";" | ",") cookie-value)
    cookie-value    =  NAME "=" VALUE [";" path] [";" domain] [";" port]
    cookie-version  =  "$Version" "=" value
    NAME            =  attr
    VALUE           =  value
    path            =  "$Path" "=" value
    domain          =  "$Domain" "=" value
    port            =  "$Port" [ "=" <"> value <"> ]
  *)

let cookie_re = Re.Str.regexp "[;,][ \t]*"
let equals_re = Re.Str.regexp_string "="

let day_in_seconds = 86400L

let get ( req : Req.t ) =
  List.fold_left
    (fun acc header ->
      let comps = Re.Str.split_delim cookie_re header in
      (* We don't handle $Path, $Domain, $Port, $Version (or $anything
             $else) *)
      let cookies = List.filter (fun s -> s.[0] != '$') comps in
      let split_pair acc nvp =
        match Re.Str.bounded_split equals_re nvp 2 with
        | [] -> StringMap.add "" "" acc
        | n :: [] -> StringMap.add n "" acc
        | n :: v :: _ -> StringMap.add n v acc
      in
      List.fold_left split_pair acc cookies
    ) StringMap.empty (StringMap.find "cookie" req.Req.req_headers)

(* TODO: Find a proper way to do that, Cohttp lib doesn't provide valid header when trying to clear header *)
let set ?secure ?http_only ~expiration ~name ~value () =
 ignore secure;
 ignore http_only;
 "Set-Cookie", Printf.sprintf "%s=%s; Max-Age=%s" name value (Int64.to_string expiration)
  (* Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0 @@
  Cohttp.Cookie.Set_cookie_hdr.make ~expiration ?secure ?http_only (name, value) *)

let clear ~name () =
  set ~name ~value:"" ~expiration:0L ()

let set ?secure ?http_only ?expiration ~name ~value =
  let expiration = 
    match expiration with
    | Some exp -> exp
    | None -> day_in_seconds
  in
  set ?secure ?http_only ~name ~value ~expiration 