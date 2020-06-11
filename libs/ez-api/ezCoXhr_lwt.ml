module Base = EzCohttp_base.Make(Cohttp_lwt_xhr.Client)

include EzRequest_lwt.Make(struct

    let get ?meth ?headers ?msg url =
      Base.get ?meth ?headers ?msg url

    let post ?meth ?content_type ?content ?headers ?msg url =
      Base.post ?meth ?content_type ?content ?headers ?msg url

  end)

let init () =
  EzEncodingJS.init ();
  EzDebugJS.init ();
  init ();
  EzRequest.log := (fun s -> Js_of_ocaml.(Firebug.console##log (Js.string s)));
  !EzRequest.log "ezCoXhr Loaded";
  ()
