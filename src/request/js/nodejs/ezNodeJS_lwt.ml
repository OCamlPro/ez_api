open Nodejs_common

let to_lwt f =
  let w, n = Lwt.wait () in
  f (Lwt.wakeup n);
  w

module Interface = struct
  let get ?meth ?headers ?msg url =
    log ?meth url msg;
    let protocol = get_protocol url in
    let options = { meth; headers } in
    to_lwt (get ?protocol ~options url)

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url =
    log ~meth url msg;
    let protocol = get_protocol url in
    let headers =
      ("Content-Type", content_type) ::
      ("Content-Length", string_of_int @@ String.length content) :: headers in
    let options = { meth = Some meth; headers = Some headers } in
    to_lwt (post ?protocol ~options url ~content)
end

include EzRequest_lwt.Make(Interface)
