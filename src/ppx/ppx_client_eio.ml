let () =
  let kind = Some `client in
  Ppx_common.async_backend := `eio;
  Ppxlib.Driver.register_transformation "ez_api_client_eio" ~impl:(Ppx_common.impl ?kind);
  Ppx_common.derivers kind;
  Ppx_common.global_deriver kind
