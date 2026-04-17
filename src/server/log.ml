let verbose = match Sys.getenv_opt "EZAPISERVER" with
  | None -> ref 0
  | Some s -> match int_of_string_opt s with
    | None -> ref 1
    | Some i -> ref i

let set_verbose i = verbose := i

let pp_time () =
  GMTime.(date_of_tm @@ Unix.gmtime @@ time ())

let rec simple_power x n =
  if n = 0 then 1 else  x * (simple_power x (n-1))

let debug ?(v=0) fmt =
  let mask_version = !verbose >= 8 in
  if (not mask_version && !verbose > v) ||
     (mask_version && ((!verbose/8) land (simple_power 2 v)) <> 0)  then
    EzDebug.printf fmt
  else Printf.ifprintf () fmt

let debugf ?(v=0) f =
  let mask_version = !verbose >= 8 in
  if (not mask_version && !verbose > v) ||
     (mask_version && ((!verbose/8) land (simple_power 2 v)) <> 0) then f ()
