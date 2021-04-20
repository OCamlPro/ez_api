let printf fmt = Printf.kprintf (fun s -> Format.eprintf "%s@." s) fmt
let log = prerr_endline
