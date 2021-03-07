type t = {
  ip_ip : string;
  mutable ip_last : float;
  mutable ip_nb : int;
  ip_country : string * string;
}

let req_ips = Hashtbl.create 1111

let register ip =
  try
    let s = Hashtbl.find req_ips ip in
    s.ip_last <- Timings.req_time ();
    s.ip_nb <- s.ip_nb + 1
  with Not_found ->
    let gi = Geoip.init_exn Geoip.GEOIP_MEMORY_CACHE in
    let country_name =
      match Geoip.country_name_by_name gi ip with
      | None -> ""
      | Some s -> s
    in
    let country_code =
      match Geoip.country_code_by_name gi ip with
      | None -> ""
      | Some s -> s
    in
    Geoip.close gi;
    let s = {
      ip_ip = ip;
      ip_last = Timings.req_time ();
      ip_nb = 1;
      ip_country = (country_name, country_code);
    } in
    Hashtbl.add req_ips ip s
