module Base = EzCohttp_base.Make(Cohttp_lwt_unix.Client)

let () = match Sys.getenv_opt "EZAPICLIENT" with
  | Some "true" | Some "1" -> Cohttp_lwt_unix.Debug.activate_debug ()
  | _ -> ()

module Interface = struct

  let get ?meth msg url ?headers f =
    let msg = match msg with "" -> None | s -> Some s in
    Lwt.async @@ fun () ->
    Lwt.map f (Base.get ?meth ?headers ?msg url)

  let post ?meth ?content_type ?content msg url ?headers f =
    let msg = match msg with "" -> None | s -> Some s in
    Lwt.async @@ fun () ->
    Lwt.map f (Base.post ?meth ?content_type ?content ?headers ?msg url)

end

include EzRequest.Make(Interface)
