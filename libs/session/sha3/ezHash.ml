let hash s = Digestif.SHA3_256.(to_raw_string @@ digest_string s)
