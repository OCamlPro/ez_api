let make ?meth ?headers ?content ?content_type url msg f =
  let msg = if msg = "" then None else (Some msg) in
  EzCurl_common.log ?meth url msg;
  let rc, data =
    try
      let r, c = EzCurl_common.init ?meth ?headers ?content ?content_type url in
      Curl.perform c;
      let rc = Curl.get_responsecode c in
      Curl.cleanup c;
      rc, Buffer.contents r
    with _ -> -1, ""
  in
  EzCurl_common.log ~meth:("RECV " ^ string_of_int rc) url msg;
  if rc >= 200 && rc < 300 then
    try f (Ok data) with _ -> ()
  else
    try f (Error (rc, Some data)) with _ -> ()

module Interface = struct

  let get ?(meth="GET") msg url ?headers f =
    make ~meth ?headers url msg f

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}")
      msg url ?headers f =
    make ~meth ?headers ~content_type ~content url msg f
end

include EzRequest.Make(Interface)
