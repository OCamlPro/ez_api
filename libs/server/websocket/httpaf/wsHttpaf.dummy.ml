let ws _fd _req ?onclose:_ ?step:_ ~react:_ ~bg:_ _id =
  Lwt.return_error `no_ws_library
