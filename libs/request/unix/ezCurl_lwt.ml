open EzRequest_lwt

let (>|=) = Lwt.(>|=)

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
  Curl.set_verbose c false;
  Curl.set_post c false;
  Curl.set_url c url;
  r,c

let make ~headers prepare url =
  let r () =
    let r, c = initialize_connection url in
    prepare c;
    Curl.set_httpheader c (
      List.map (fun (name, value) -> Printf.sprintf "%s: %s" name value) headers);
    Curl_lwt.perform c >|= fun _code ->
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    let data = Buffer.contents r in
    if rc >= 200 && rc < 300 then Ok data
    else Error (rc, Some data) in
  Lwt.catch r (fun exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))

include Make(struct
    let get ?meth:_ ?(headers=[]) ?msg:_ url =
      make ~headers (fun c -> Curl.set_post c false) url

    let post ?(meth="POST") ?(content_type = "application/json") ?(content="{}") ?(headers=[])
        ?msg:_ url =
      let headers = ("Content-Type", content_type) :: headers in
      make ~headers (fun c ->
          if meth = "PUT" then Curl.set_put c true else Curl.set_post c true;
          Curl.set_postfields c content;
          Curl.set_postfieldsize c (String.length content);
        ) url
end)
