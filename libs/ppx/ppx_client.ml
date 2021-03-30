let () =
  Ppxlib.Driver.register_transformation "ez_api_client" ~impl:(Ppx_common.impl ~kind:`client)
