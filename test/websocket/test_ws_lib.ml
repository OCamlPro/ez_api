open EzAPI

let service : (string, string, exn, no_security) ws_service0 =
  ws_service
    ~input:(Json Json_encoding.string)
    ~output:(Json Json_encoding.string)
    Path.root
