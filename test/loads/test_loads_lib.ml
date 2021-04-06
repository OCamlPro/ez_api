let output = Json_encoding.(obj1 (req "success" (constant "true")))
let section = EzAPI.Doc.section "main"
let arg = EzAPI.Arg.string ~example:"machin" "id"
let params = [ EzAPI.Param.string "foo" ]

let h _s =
  EzAPIServerUtils.return_ok ()
[@@get {path="/test/{arg}"; output; section; params}]

let ppx_dir = EzLoads_offset.register ppx_dir
