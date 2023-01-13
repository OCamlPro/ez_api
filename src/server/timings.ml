(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type period = {
  name : string;
  mutable prev : int;
  nb : int array;
  dt : float array;
}

type t = {
  first : float;
  mutable last : float;
  minutes : period;
  hours : period;
  days : period;
  all : period;
}

(* Move to current time, cleaning all statistics that are absolete *)
let forward_period p pos =
  let size = Array.length p.nb in
  if pos - p.prev > size then begin
    Array.fill p.nb 0 size 0;
    Array.fill p.dt 0 size 0.;
    p.prev <- pos
  end
  else begin
    let rec iter p pos size =
      if pos > p.prev then
        let new_prev = p.prev+1 in
        p.prev <- new_prev;
        let n = new_prev mod size in
        p.nb.(n) <- 0;
        p.dt.(n) <- 0.;
        iter p pos size
    in
    iter p pos size
  end;
  pos mod size

let add_period p pos dt =
  try
    let pos = forward_period p pos in
    p.nb.(pos) <- p.nb.(pos) + 1;
    p.dt.(pos) <- p.dt.(pos) +. dt
  with exn ->
    Printf.eprintf "Timings: exn %s discarded\n%!"
      (Printexc.to_string exn)

let add t t1 dt =
  let second = int_of_float (t1 -. t.first) in
  let minute = second / 60 in
  let hour = minute / 60 in
  let day = hour / 24 in
  add_period t.minutes minute dt;
  add_period t.hours hour dt;
  add_period t.days day dt;
  add_period t.all 0 dt;
  t.last <- t1;
  ()

let create_period name n =
  {
    name;
    prev = 0;
    nb = Array.make n 0;
    dt = Array.make n 0.;
  }

let copy_period p pos =
  let size = Array.length p.nb in
  let p2 = create_period p.name size in
  for i = 0 to size - 1 do
    let n = (size+pos-i) mod size in
    p2.nb.(i) <- p.nb.(n);
    p2.dt.(i) <- p.dt.(n);
  done;
  p2

let create first =
  {
    first; last = first;
    minutes = create_period "minutes" 60;
    hours = create_period "hours" 24;
    days = create_period "days" 30;
    all = create_period "all" 1;
  }

(* return timing information with:
     * arrays are sorted from current time to previous time
       minutes.(0) = current, minutes.(1) = one minute ago, etc.
     * arrays are cleaned-up to current time (i.e. arrays = 0 if no event
       happened in the last minutes)
*)
let get t1 t =
  let second = int_of_float (t1 -. t.first) in
  let minute = second / 60 in
  let hour = minute / 60 in
  let day = hour / 24 in
  ignore (forward_period t.minutes minute : int);
  ignore (forward_period t.hours hour : int);
  ignore (forward_period t.days day : int);
  { t with
    minutes = copy_period t.minutes minute;
    hours = copy_period t.hours hour;
    days = copy_period t.days day;
  }

type timings = {
  mutable timings_ok : t array;
  mutable timings_fail : t array;
}

let timings = {
  timings_ok = [||];
  timings_fail = [||];
}

let add_timing n ok t dt =
  if ok then add timings.timings_ok.(n) t dt
  else add timings.timings_fail.(n) t dt

let init t0 nservices =
  timings.timings_ok <-Array.init nservices
      (fun _ -> create t0);
  timings.timings_fail <- Array.init nservices
      (fun _ -> create t0)
