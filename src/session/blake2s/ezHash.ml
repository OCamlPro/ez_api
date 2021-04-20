let hash s = Digestif.BLAKE2S.(to_raw_string @@ digest_string s)
