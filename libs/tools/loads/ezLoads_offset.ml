let section = EzAPI.Doc.section "EzLoads"

let service_offset : (float * float, exn, EzAPI.no_security) EzAPI.service0 =
  EzAPI.service ~section ~output:Json_encoding.(tup2 float float)
    EzAPI.Path.(root // "ezloads_offset")

let handler_offset req _ () =
  EzAPIServerUtils.return_ok (req.EzAPI.Req.req_time, Unix.gettimeofday ())

let register dir =
  EzAPIServerUtils.register service_offset handler_offset dir
