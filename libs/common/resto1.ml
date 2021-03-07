(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)



module Mime = struct
  end

type mime = Mime.t

module Param = struct
end

module Security = struct

end



module Err = struct
end



module Make(Repr : Json_repr.Repr) = struct

  include Json_encoding.Make(Repr)

  type json = Repr.value

  let forge_request
    : type a i . (a, i, _, _, _) service -> a -> i -> string list * Repr.value
    = fun s args content ->
      let json : json = match s.input with
        | Json enc -> construct enc content
        | Empty -> Repr.repr (`String "")
        | Raw _ -> Repr.repr (`String content) in
      Path.forge s.path args, json

  let read_answer
    : type a o . (a, _, o, _, _) service -> Repr.value -> (o, string) result
    = fun s json ->
      try Ok (destruct s.output json)
      with exn ->
        Error (Format.asprintf "%a" (fun ppf -> Json_encoding.print_error ppf) exn)
end

include Make(Json_repr.Ezjsonm)
