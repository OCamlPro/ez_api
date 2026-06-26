module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let ansi_color = match Sys.getenv_opt "EZAPICOLOR" with
  | None | Some "0" | Some "false" -> ref false
  | _ -> ref true

let apply_ansi_color ?(thickness=0) code =
  if not !ansi_color then "", ""
  else Format.sprintf "\027[%d;%dm" thickness code, "\027[0m"
