let () = Mirage_crypto_rng_unix.use_default ()

  let https = Some (fun uri raw ->
    let authenticator ?ip:_ ~host:_ _ = Ok None in
    let host = Option.map (fun x -> Domain_name.(host_exn (of_string_exn x))) @@ Uri.host uri in
    let config = Result.get_ok @@ Tls.Config.client ~authenticator () in
    Tls_eio.client_of_flow ?host config raw)
