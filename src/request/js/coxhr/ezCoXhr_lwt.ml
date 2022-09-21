module Base = EzCohttp_base.Make(Cohttp_lwt_jsoo.Client)

module Interface = struct
  let get ?meth ?headers ?msg url =
    Base.get ?meth ?headers ?msg url

  let post ?meth ?content_type ?content ?headers ?msg url =
    Base.post ?meth ?content_type ?content ?headers ?msg url
end

include EzRequest_lwt.Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezCoXhr Loaded"
