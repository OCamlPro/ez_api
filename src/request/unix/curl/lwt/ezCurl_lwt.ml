(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let make ?msg ?meth ?headers ~url l =
  Verbose.request ?msg ?meth ~content:(EzCurl_common.payload_to_string l) url;
  let r () =
    let r, c = EzCurl_common.init ?meth ?headers ~url l in
    Lwt.map (fun _code ->
        let rc = Curl.get_responsecode c in
        Curl.cleanup c;
        let data = Buffer.contents r in
        Verbose.response ?msg ~code:rc ~content:data url;
        if rc >= 200 && rc < 300 then Ok data
        else Error (rc, Some data))
      (Curl_lwt.perform c) in
  Lwt.catch r (function
      | Curl.CurlException (_, i, s) -> Lwt.return (Error (i, Some s))
      | exn -> Lwt.return (Error (-1, Some (Printexc.to_string exn))))

module Interface = struct
  let get ?(meth="GET") ?headers ?msg url =
    make ?msg ~meth ?headers ~url []

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers
      ?msg url =
    make ?msg ~meth ?headers ~url [ "", `content content, Some content_type ]
end

include EzRequest_lwt.Make(Interface)
