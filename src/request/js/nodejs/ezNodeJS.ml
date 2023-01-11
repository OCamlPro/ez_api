(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Nodejs_common

module Interface = struct
  let get ?meth ?headers ?msg url f =
    log ?meth url msg;
    let protocol = get_protocol url in
    let options = { meth; headers } in
    get ?protocol ~options url f

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url  f =
    log ~meth url msg;
    let protocol = get_protocol url in
    let headers =
      ("Content-Type", content_type) ::
      ("Content-Length", string_of_int @@ String.length content) :: headers in
    let options = { meth = Some meth; headers = Some headers } in
    post ?protocol ~options url ~content f
end

include EzRequest.Make(Interface)
