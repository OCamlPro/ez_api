module Interface = struct

  let get ?meth:_ _msg _url ?headers:_ f =
    f @@ Error (-2, Some "No http client loaded")

  let post ?meth:_ ?content_type:_ ?content:_
      _msg _url ?headers:_ f =
    f @@ Error (-2, Some "No http client loaded")

end

include EzRequest.Make(Interface)
