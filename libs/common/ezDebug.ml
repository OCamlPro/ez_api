
let printer = ref (fun s ->
                  Printf.eprintf "%s\n%!" s)

let printf fmt =
  Printf.kprintf (!printer) fmt
