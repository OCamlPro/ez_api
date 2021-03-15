let verbose = match Sys.getenv_opt "EZAPICLIENT" with
  | Some "true" | Some "1" -> true
  | _ -> false

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Printf.printf "[>%s %s %s ]\n%!" msg meth url

let writer_callback a d =
  Buffer.add_string a d;
  String.length d

let initialize_connection url =
  let r = Buffer.create 16384
  and c = Curl.init () in
  Curl.set_timeout c 30;      (* Timeout *)
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  Curl.set_writefunction c (writer_callback r);
  Curl.set_tcpnodelay c true;
  Curl.set_verbose c verbose;
  Curl.set_post c false;
  Curl.set_url c url;
  r,c

let make ?msg ~headers prepare url =
  let r () =
    let r, c = initialize_connection url in
    prepare c;
    Curl.set_httpheader c (
      List.map (fun (name, value) -> Printf.sprintf "%s: %s" name value) headers);
    Lwt.map (fun _code ->
        let rc = Curl.get_responsecode c in
        Curl.cleanup c;
        let data = Buffer.contents r in
        log ~meth:("RECV " ^ string_of_int rc) url msg;
        if rc >= 200 && rc < 300 then Ok data
        else Error (rc, Some data))
      (Curl_lwt.perform c) in
  Lwt.catch r (fun exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))

module Interface = struct
  let get ?(meth="GET") ?(headers=[]) ?msg url =
    log ~meth url msg;
    make ?msg ~headers (fun c -> Curl.set_post c false) url

  let post ?(meth="POST") ?(content_type = "application/json") ?(content="{}") ?(headers=[])
      ?msg url =
    log ~meth url msg;
    let headers = ("Content-Type", content_type) :: headers in
    make ?msg ~headers (fun c ->
        if meth = "PUT" then Curl.set_put c true else Curl.set_post c true;
        Curl.set_postfields c content;
        Curl.set_postfieldsize c (String.length content);
      ) url
end

include EzRequest_lwt.Make(Interface)
