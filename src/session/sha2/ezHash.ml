let hash s = Digestif.SHA256.(to_raw_string @@ digest_string s)
