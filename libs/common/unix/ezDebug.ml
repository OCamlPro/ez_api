let printf fmt = Printf.kprintf (fun s -> Format.eprintf "%s@.") fmt
let log = prerr_endline
