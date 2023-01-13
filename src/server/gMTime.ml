(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Times, in GMT, not in localtime () ! *)

(* Computes the difference between GMT and localtime *)
let diff =
  let time = Unix.gettimeofday () in
  let gmt,_ = Unix.mktime (Unix.gmtime time) in
  let local,_ = Unix.mktime (Unix.localtime time) in
  local -. gmt

(* translate from localtime *)
let of_local local = local -. diff

(* translate to localtime *)
let to_local gmt = gmt +. diff

let time () = Unix.gettimeofday ()

let tm_of_date date = (* example: 2018-01-31T13:12:11Z *)
  let datelen = String.length date in
  assert (datelen >= 19);
  assert (date.[4] = '-');
  assert (date.[7] = '-');
  assert (date.[10] = 'T');
  assert (date.[13] = ':');
  assert (date.[16] = ':');
  let tm_year = int_of_string (String.sub date 0 4) - 1900 in
  let tm_mon = int_of_string (String.sub date 5 2) - 1 in
  let tm_mday = int_of_string (String.sub date 8 2) in
  let tm_hour = int_of_string (String.sub date 11 2) in
  let tm_min = int_of_string (String.sub date 14 2) in
  let tm_sec = int_of_string (String.sub date 17 2) in
  Unix.({
           tm_year; tm_mon; tm_mday;
           tm_hour; tm_min; tm_sec;
           (* recomputed *)
           tm_wday = 0; tm_yday = 0; tm_isdst = true;
  })

let date_of_tm tm =
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
                 (tm.Unix.tm_year+1900)
                 (tm.Unix.tm_mon+1)
                 tm.Unix.tm_mday
                 tm.Unix.tm_hour
                 tm.Unix.tm_min
                 tm.Unix.tm_sec

let time_of_tm tm =
  let time, _tm = Unix.mktime tm in
  time

let tm_of_time time =
  Unix.localtime time

let nsecs_per_day = 3600 * 24
