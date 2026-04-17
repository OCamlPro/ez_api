include EzWsEioTypes.Types

let err = "ezWs not implemented, either install httpun-ws-lwt-unix or websocket-lwt-unix"

let connect ?msg:_ ?protocols:_ ?error:_ ~net:_ ~sw:_ ~react:_ _url = Error err

let connect0 ?msg:_ ?protocols:_ ?error:_ ~net:_ ~sw:_ ~react:_ _base _service = Error err
