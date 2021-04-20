let () =
  Ppxlib.Driver.register_transformation "ez_api_server" ~impl:(Ppx_common.impl ~kind:`server)
