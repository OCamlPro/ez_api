let meth = function
  | `Other ("patch" | "PATCH" | "Patch") -> Some `PATCH
  | `GET | `PUT | `OPTIONS | `POST | `DELETE | `HEAD as m -> Some m
  | _ -> None

let mk_uri ~meth ~target ~header =
  match target with
  | "*" ->
    begin match header "host" with
      | None -> Uri.of_string ""
      | Some host ->
        let host_uri = Uri.of_string ("//"^host) in
        let uri = Uri.(with_host (of_string "") (host host_uri)) in
        Uri.(with_port uri (port host_uri))
    end
  | authority when meth = `CONNECT -> Uri.of_string ("//" ^ authority)
  | path ->
    let uri = Uri.of_string path in
    begin match Uri.scheme uri with
      | Some _ -> (* we have an absoluteURI *)
        Uri.(match path uri with "" -> with_path uri "/" | _ -> uri)
      | None ->
        let empty = Uri.of_string "" in
        let empty_base = Uri.of_string "///" in
        let pqs = match Stringext.split ~max:2 path ~on:'?' with
          | [] -> empty_base
          | [path] ->
            Uri.resolve "http" empty_base (Uri.with_path empty path)
          | path::qs::_ ->
            let path_base =
              Uri.resolve "http" empty_base (Uri.with_path empty path)
            in
            Uri.with_query path_base (Uri.query_of_encoded qs)
        in
        let uri = match header "host" with
          | None -> Uri.(with_scheme (with_host pqs None) None)
          | Some host ->
            let host_uri = Uri.of_string ("//"^host) in
            let uri = Uri.with_host pqs (Uri.host host_uri) in
            Uri.with_port uri (Uri.port host_uri)
        in
        uri
    end

let peer = function
  | Unix.ADDR_INET (iaddr, _port) -> Some (Unix.string_of_inet_addr iaddr)
  | Unix.ADDR_UNIX _ -> None

let register_ip time r = Option.iter (Ip.register time) r.EzAPI.Req.req_ip
