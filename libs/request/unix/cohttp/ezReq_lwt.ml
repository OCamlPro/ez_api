module Base = EzCohttp_base.Make(Cohttp_lwt_unix.Client)

module Interface = struct

  let get ?meth ?headers ?msg url =
    Base.get ?meth ?headers ?msg url

  let post ?meth ?content_type ?content ?headers ?msg url =
    Base.post ?meth ?content_type ?content ?headers ?msg url

end

include EzRequest_lwt.Make(Interface)
