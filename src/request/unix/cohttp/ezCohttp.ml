module Base = EzCohttp_base.Make(Cohttp_lwt_unix.Client)

let () =
  let cb v  = if v land 4 <> 0 then Cohttp_lwt_unix.Debug.activate_debug () in
  Verbose.callback := cb;
  cb !Verbose.v

module Interface = struct

  let get ?meth ?headers ?msg url f =
    Lwt.async @@ fun () ->
    Lwt.map f (Base.get ?meth ?headers ?msg url)

  let post ?meth ?content_type ?content ?headers ?msg url f =
    Lwt.async @@ fun () ->
    Lwt.map f (Base.post ?meth ?content_type ?content ?headers ?msg url)

end

include EzRequest.Make(Interface)
