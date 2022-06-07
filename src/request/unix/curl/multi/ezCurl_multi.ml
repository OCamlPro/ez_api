let make ?msg ?meth ?content ?content_type ?headers url =
  EzCurl_common.log ?meth url msg;
  let r () =
    let r, c = EzCurl_common.init ?meth ?content ?content_type ?headers url in
    Lwt.map (fun _code ->
        let rc = Curl.get_responsecode c in
        Curl.cleanup c;
        let data = Buffer.contents r in
        EzCurl_common.log ~meth:("RECV " ^ string_of_int rc) url msg;
        if rc >= 200 && rc < 300 then Ok data
        else Error (rc, Some data))
      (Curl_lwt.perform c) in
  Lwt.catch r (fun exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))

module Interface = struct
  let get ?(meth="GET") ?headers ?msg url =
    make ?msg ~meth ?headers url

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers
      ?msg url =
    make ?msg ~meth ?headers ~content ~content_type url
end

include EzRequest_lwt.Make(Interface)
