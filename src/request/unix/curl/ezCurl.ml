let make ?meth ?headers ?content ?content_type ?msg url f =
  EzCurl_common.log ?meth url msg;
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n%s@." (Option.value ~default:"" content);
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
  if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." data;
  if rc >= 200 && rc < 300 then try f (Ok data) with _ -> ()
  else try f (Error (rc, Some data)) with _ -> ()

module Interface = struct

  let get ?(meth="GET") ?headers ?msg url f =
    make ~meth ?headers ?msg url f

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}")
      ?headers ?msg url  f =
    make ~meth ?headers ?msg ~content_type ~content url f
end

include EzRequest.Make(Interface)
