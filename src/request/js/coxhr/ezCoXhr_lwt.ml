(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

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
