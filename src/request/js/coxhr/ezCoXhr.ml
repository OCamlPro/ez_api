open Lwt.Infix
open EzRequest

module Base = EzCohttp_base.Make(Cohttp_lwt_jsoo.Client)

module Interface = struct
  let get ?meth ?headers ?msg url f =
    Lwt.async @@ fun () ->
    Base.get ?meth ?headers ?msg url >|= f

  let post ?meth ?content_type ?content ?headers ?msg url f =
    Lwt.async @@ fun () ->
    Base.post ?meth ?content_type ?content ?headers ?msg url >|= f
end

include Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezCoXhr Loaded"
