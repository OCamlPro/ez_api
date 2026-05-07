let meth = function
  | `Other ("patch" | "PATCH" | "Patch") -> Some `PATCH
  | `GET | `PUT | `OPTIONS | `POST | `DELETE | `HEAD as m -> Some m
  | _ -> None

let debug ~meth ~target ~headers =
  Log.debug "[%t] REQUEST: %s %S" GMTime.pp_now meth target;
  Log.debugf ~v:1 @@ fun () ->
  List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value) headers

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

let register_ip ~header time = function
  | Unix.ADDR_INET (iaddr, _port) ->
    let ip = match header "x-forwarded-for" with
      | None -> Unix.string_of_inet_addr iaddr
      | Some ip -> ip in
    Ip.register time ip
  | Unix.ADDR_UNIX _ -> ()
