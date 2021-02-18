open Lwt.Infix
open EzRequest

module Base = EzCohttp_base.Make(Cohttp_lwt_xhr.Client)

module Interface = struct

  let get ?meth msg url ?headers f =
    let msg = match msg with "" -> None | s -> Some s in
    Lwt.async @@ fun () ->
    Base.get ?meth ?headers ?msg url >|= f

  let post ?meth ?content_type ?content msg url ?headers f =
    let msg = match msg with "" -> None | s -> Some s in
    Lwt.async @@ fun () ->
    Base.post ?meth ?content_type ?content ?headers ?msg url >|= f

end

include Make(Interface)

let () = EzDebug.log "ezCoXhr Loaded"
