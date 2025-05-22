let wrap_error e =
  let aux () = Format.asprintf "%a" Httpun_client.pp_error e in
  match e with
  | `http (code, content) -> code, Some content
  | `timeout _ -> 408, Some (aux ())
  | `invalid_url _ -> 400, Some (aux ())
  | _ -> -1, Some (aux ())

module Interface = struct
  let get ?(meth="GET") ?headers ?msg:_ url =
    let meth = EzAPI.Meth.of_string meth in
    Lwt.map (Result.map_error wrap_error) @@ Httpun_client.call ~meth ?headers url

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers
      ?msg:_ url =
    let meth = EzAPI.Meth.of_string meth in
    Lwt.map (Result.map_error wrap_error) @@
    Httpun_client.call ~meth ?headers ~content ~content_type url
end

include EzRequest_lwt.Make(Interface)
