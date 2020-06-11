open Lwt.Infix
open EzRequest

module Base = EzCohttp_base.Make(Cohttp_lwt_xhr.Client)

include Make(struct

    let xhr_get ?meth _msg url ?headers f =
      Lwt.async @@ fun () ->
      Base.get ?meth ?headers url >|= function
      | Ok body -> f (CodeOk body)
      | Error (code, content) -> f (CodeError (code, content))

    let xhr_post ?meth ?content_type ?content _msg url ?headers f =
      Lwt.async @@ fun () ->
      Base.post ?meth ?content_type ?content ?headers url >|= function
      | Ok body -> f (CodeOk body)
      | Error (code, content) -> f (CodeError (code, content))

  end)

let init () =
  EzEncodingJS.init ();
  EzDebugJS.init ();
  init ();
  EzRequest.log := (fun s -> Js_of_ocaml.(Firebug.console##log (Js.string s)));
  !EzRequest.log "ezCoXhr Loaded";
  ()

let () = init ()
