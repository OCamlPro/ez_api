let verbose = match Sys.getenv_opt "EZAPISERVER" with
  | None -> ref 0
  | Some s -> match int_of_string_opt s with
    | None -> ref 1
    | Some i -> ref i

let set_verbose i = verbose := i

let rec simple_power x n =
  if n = 0 then 1 else  x * (simple_power x (n-1))

let debug ?(v=0) fmt =
  let mask_version = !verbose >= 8 in
  if (not mask_version && !verbose > v) ||
     (mask_version && ((!verbose/8) land (simple_power 2 v)) <> 0)  then
    EzDebug.printf fmt
  else Format.ifprintf Format.err_formatter fmt

let debugf ?(v=0) f =
  let mask_version = !verbose >= 8 in
  if (not mask_version && !verbose > v) ||
     (mask_version && ((!verbose/8) land (simple_power 2 v)) <> 0) then f ()


let content_length_to_string n =
  let rec aux l r d = match l with
    | [] -> assert false
    | [ s ] -> Format.sprintf "%d%s" n s
    | h :: tl ->
      let d2 = d / 1024 in
      if d2 > 10 then aux tl (d mod 1024) d2 else
        let d = if r >= 512 then d+1 else d in
        Format.sprintf "%d%s" d h in
  aux [ ""; "ko"; "Mo"; "To" ] 0 n

let pp_content_length fmt n =
  Format.fprintf fmt "%s" (content_length_to_string n)
