let () =
  Ppxlib.Driver.register_transformation "ez_api" ~impl:Ppx_common.impl
