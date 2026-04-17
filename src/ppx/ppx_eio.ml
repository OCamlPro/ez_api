let () =
  Ppx_common.async_backend := `eio;
  Ppxlib.Driver.register_transformation "ez_api_eio" ~impl:Ppx_common.impl;
  Ppx_common.derivers None;
  Ppx_common.global_deriver None
