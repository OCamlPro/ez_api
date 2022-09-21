module Base = EzCohttp_base.Make(Cohttp_lwt_unix.Client)

let () =
  let cb v  = if v land 4 <> 0 then Cohttp_lwt_unix.Debug.activate_debug () in
  Verbose.callback := cb;
  cb !Verbose.v

module Interface = struct

  let get ?meth ?headers ?msg url =
    Base.get ?meth ?headers ?msg url

  let post ?meth ?content_type ?content ?headers ?msg url =
    Base.post ?meth ?content_type ?content ?headers ?msg url

end

include EzRequest_lwt.Make(Interface)
