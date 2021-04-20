module Interface = struct

  let get ?meth:_ ?headers:_ ?msg:_ _url f =
    f @@ Error (-2, Some "No http client loaded")

  let post ?meth:_ ?content_type:_ ?content:_ ?headers:_ ?msg:_ _url f =
    f @@ Error (-2, Some "No http client loaded")

end

include EzRequest.Make(Interface)
