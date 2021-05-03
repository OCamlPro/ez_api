let req_ips = Hashtbl.create 1111

let register =
  let first = ref true in
  fun _ _ ->
    if !first then (
      first := false;
      Format.eprintf
        "\027[0;33mwarning: geoip not installed\027[0m\n\
         If you need ip information try: `opam install geoip`@.")
