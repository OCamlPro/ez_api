module Interface = struct
  let get ?(meth="GET") ?headers ?msg:_ url =
    let meth = EzAPI.Meth.of_string meth in
    Httpun_client.call ~meth ?headers url

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers
      ?msg:_ url =
    let meth = EzAPI.Meth.of_string meth in
    Httpun_client.call ~meth ?headers ~content ~content_type url
end

include EzRequest_lwt.Make(Interface)
