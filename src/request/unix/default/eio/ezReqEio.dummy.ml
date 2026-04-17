module Interface = struct

  let get ?meth:_ ?headers:_ ?msg:_ ~net:_ ~sw:_ _url =
    -2, "No http client loaded", []

  let post ?meth:_ ?headers:_ ?msg:_ ?content_type:_ ?content:_ ~net:_ ~sw:_ _url =
    -2, "No http client loaded", []
end

include EzRequestEio.Make(Interface)
