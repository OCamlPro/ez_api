(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let average f = function
  | [] -> 0.
  | l ->
    let wsum = List.fold_left (fun acc x -> acc +. (f x)) 0. l in
    wsum /. (float_of_int @@ List.length l)

let median f = function
  | [] -> 0.
  | l ->
    let l = List.sort (fun x1 x2 -> compare (f x1) (f x2)) l in
    let n = List.length l in
    if n mod 2 = 0 then (f (List.nth l (n/2 - 1)) +. f (List.nth l (n/2))) /. 2.
    else f (List.nth l (n/2))

let std_dev f = function
  | [] -> 0.
  | l ->
    let av = average f l in
    let wsum2 = List.fold_left (fun acc x -> ((f x) -. av) ** 2. +. acc) 0. l in
    sqrt @@ wsum2 /. (float_of_int @@ List.length l)

type service = {
  s_path : string;
  s_args : (string * string list) list; [@assoc]
  s_params : (string * string option list) list; [@assoc]
  s_iter : int; [@dft 1]
  s_parallel : int option; [@opt]
} [@@deriving encoding]

type config = {
  c_iter : int; [@ddft 1]
  c_parallel : int option; [@opt]
  c_services : service list
} [@@deriving encoding]

let request ?input ?(offset=false) url =
  let t0 = Unix.gettimeofday () in
  let handle = function
    | Ok s ->
      let t3 = Unix.gettimeofday () in
      let offset =
        if offset then
          let t1, t2 = EzEncoding.destruct Json_encoding.(tup2 float float) s in
          Some ((t1 -. t0) *. 1000., (t2 -. t1) *. 1000., (t3 -. t2) *. 1000.)
        else None in
      Ok ((t3 -. t0) *. 1000., offset)
    | Error _ -> Error () in
  match input with
  | None -> Lwt.map handle (EzReq_lwt.get (EzAPI.URL url))
  | Some content -> Lwt.map handle (EzReq_lwt.post ~content (EzAPI.URL url))

let service_urls host s =
  let paths = match s.s_args with
    | [] -> [ host ^ s.s_path, true ]
    | l ->
      List.fold_left (fun acc (k, vs) ->
          acc @ List.map (fun v ->
              let p = host ^ Str.(replace_first (regexp @@ "{" ^ k ^ "}") v s.s_path) in
              p, true) vs
        ) [] l in
  let urls = List.fold_left (fun acc (k, vs) ->
      List.flatten @@
      List.map (fun (u, first) ->
          List.map (function
              | None -> u, first
              | Some v ->
                let pr = if first then "?" else "&" in
                u ^ pr ^ k ^ "=" ^ v, false) vs) acc)
      paths s.s_params in
  List.map fst urls

let divide_list n l =
  let rec aux acc tmp i = function
    | [] -> if tmp = [] then List.rev acc else List.rev (tmp :: acc)
    | (h :: t) as l ->
      if i >= n then aux ((List.rev tmp) :: acc) [] 0 l
      else aux acc (h :: tmp) (i+1) t in
  aux [] [] 0 l

let service_requests host s =
  let urls = service_urls host s in
  let urls = List.flatten @@ List.map (fun u ->
      List.init s.s_iter (fun i -> u, i)) urls in
  let handle (c, ldt) = function
    | Ok dt -> c+1, dt :: ldt
    | Error () -> c+1, ldt in
  let offset = if s.s_path = "/ezloads_offset" then true else false in
  match s.s_parallel with
  | None ->
    Lwt.map
      (List.fold_left handle (0, []))
      (Lwt_list.map_s (fun (url, _i) -> request ~offset url) urls)
  | Some max_p ->
    let n = if max_p = 0 then 0 else List.length urls / (max_p - 1) in
    if n = 0 then
      Lwt.map
        (List.fold_left handle (0, []))
        (Lwt_list.map_p (fun (url, _i) -> request ~offset url) urls)
    else
      let urls_divided = divide_list max_p urls in
      Lwt_list.fold_left_s (fun acc urls ->
          Lwt.map
            (List.fold_left handle acc)
            (Lwt_list.map_p (fun (url, _i) -> request ~offset url) urls)) (0, []) urls_divided

type stats = {
  st_path : string;
  st_tries : int;
  st_ok : int;
  st_average : float;
  st_median : float;
  st_stdev : float;
  st_min : float;
  st_max : float;
  st_offset_in : float option;
  st_offset_dir : float option;
  st_offset_out : float option;
} [@@deriving encoding]

let compute_stats st_path st_tries dt =
  let st_offset_in, st_offset_dir, st_offset_out =
    if List.exists (function (_, None) -> true | _ -> false) dt then None, None, None
    else
      let l = List.filter_map (function (_, Some x) -> Some x | _ -> None) dt in
      Some (average (fun (x, _, _) -> x) l), Some (average (fun (_, x, _) -> x) l),
      Some (average (fun (_, _, x) -> x) l) in
  let st_ok = List.length dt in
  {
    st_path; st_tries; st_ok;
    st_max = if st_ok = 0 then 0. else List.fold_left (fun acc x -> max acc (fst x)) 0. dt;
    st_min = if st_ok = 0 then 0. else List.fold_left (fun acc x -> min acc (fst x)) max_float dt;
    st_average = average fst dt;
    st_median = median fst dt;
    st_stdev = std_dev fst dt;
    st_offset_in;
    st_offset_dir;
    st_offset_out;
  }

let print_stats s =
  if s.st_ok = 0 then
    Format.printf "%s -> %d / %d@." s.st_path s.st_ok s.st_tries
  else
    Format.printf "%s -> %d / %d\n  average: %f ms\n  median: %f ms\n  \
                   standard deviation: %f ms\n  min: %f ms\n  max: %f ms%s%s%s@."
      s.st_path s.st_ok s.st_tries s.st_average s.st_median s.st_stdev
      s.st_min s.st_max
      (match s.st_offset_in with None -> "" | Some f -> Format.sprintf "\n  input offset: %f ms" f)
      (match s.st_offset_dir with None -> "" | Some f -> Format.sprintf "\n  directory offset: %f ms" f)
      (match s.st_offset_out with None -> "" | Some f -> Format.sprintf "\n  output offset: %f ms" f)

module SMap = Map.Make(String)

let run host c =
  let services = List.flatten @@
    List.init c.c_iter (fun i -> List.map (fun s -> s, i) c.c_services) in
  let handle_result sm (path, (c, dt)) =
    SMap.update path
      (function
        | None -> Some (c, dt)
        | Some (acc_c, acc_dt) -> Some (acc_c + c, acc_dt @ dt)) sm in
  let handle_acc l =
    let l = SMap.bindings @@ List.fold_left handle_result SMap.empty l in
    List.map (fun (path, (c, dt)) ->
        let st = compute_stats path c dt in
        print_stats st;
        st) l in
  let p = match c.c_parallel with
    | None ->
      Lwt_list.map_s (fun (s, _i) ->
          Lwt.map (fun r -> s.s_path, r) (service_requests host s)) services
    | Some max_p ->
      let n = if max_p = 0 then 0 else List.length services / (max_p - 1) in
      if n = 0 then
        Lwt_list.map_p (fun (s, _i) ->
            Lwt.map (fun r -> s.s_path, r) (service_requests host s)) services
      else
        let services_divided = divide_list max_p services in
        Lwt_list.fold_left_s (fun acc services ->
            Lwt.map
              (fun x -> acc @ x)
              (Lwt_list.map_p (fun (s, _i) ->
                   Lwt.map (fun r -> s.s_path, r) (service_requests host s)) services))
          [] services_divided in
  Lwt.map handle_acc p

let services_from_sections sections =
  let open EzAPI in
  List.flatten @@
  List.map (fun sec ->
      List.map (fun doc ->
          let s_args = List.map (fun a ->
              a.Arg.name, [ match a.Arg.example with None -> "" | Some e -> e ])
              doc.Doc.doc_args in
          let s_params = List.rev @@ List.fold_left (fun acc p ->
              let params = None :: List.map Option.some p.Param.param_examples in
              (p.Param.param_id, params) :: acc) [] doc.Doc.doc_params in
          { s_path = doc.Doc.doc_path; s_args; s_params; s_iter = 1; s_parallel = None })
        sec.Doc.section_docs
    ) sections

let execute sections =
  let run_file = ref None in
  let create_file = ref None in
  let host = ref "http://localhost:8080" in
  let output = ref None in
  let specs = [
    "--run", Arg.String (fun s -> run_file := Some s), "Run loads with a config file";
    "--file", Arg.String (fun s -> create_file := Some s), "Create a dummy config file from an API Server library";
    "--host", Arg.Set_string host, "Host url (default: 'http://localhost:8080')";
    "--output", Arg.String (fun s -> output := Some s), "Output stats in file"
  ] in
  Arg.parse specs (fun _s -> ()) "ez-loads";
  match !create_file with
  | Some filename ->
    let c_services = services_from_sections sections in
    let oc = open_out filename in
    output_string oc
      (EzEncoding.construct ~compact:false config_enc {c_services; c_iter=1; c_parallel=None});
    close_out oc
  | None ->
    match !run_file with
    | None -> ()
    | Some filename ->
      let ic = open_in filename in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      EzLwtSys.run (fun () ->
          Lwt.bind (run !host @@ EzEncoding.destruct config_enc s)
            (fun l ->
               match !output with
               | None -> Lwt.return_unit
               | Some filename ->
                 let oc = open_out filename in
                 output_string oc
                   (EzEncoding.construct ~compact:false (Json_encoding.list stats_enc) l);
                 close_out oc;
                 Lwt.return_unit))
