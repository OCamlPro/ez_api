let () =
  let kind = Some `request in
  Ppx_common.async_backend := `eio;
  Ppxlib.Driver.register_transformation "ez_api_req_eio" ~impl:(Ppx_common.impl ?kind);
  Ppx_common.derivers kind;
  Ppx_common.global_deriver kind
