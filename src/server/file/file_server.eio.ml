open EzAPIServerEio
open File_server_arg

let () =
  Arg.parse specs conf "ezServe - Simple ez-api file server\nusage: ezserve [OPTIONS] [<addr:port>]";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run ?addr:!addr ~env ~sw [ !port, Root (!root, Some !default) ]
