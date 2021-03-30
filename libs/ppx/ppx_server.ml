let () =
  Ppxlib.Driver.register_transformation "ez-api-server" ~impl:(Ppx_common.impl ~kind:`server)
