(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Interface = struct

  let get ?meth:_ ?headers:_ ?msg:_ _url f =
    f @@ Error (-2, Some "No http client loaded")

  let post ?meth:_ ?content_type:_ ?content:_ ?headers:_ ?msg:_ _url f =
    f @@ Error (-2, Some "No http client loaded")

end

include EzRequest.Make(Interface)
