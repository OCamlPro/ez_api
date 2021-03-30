let () =
  Ppxlib.Driver.register_transformation "ez-api" ~impl:Ppx_common.impl
