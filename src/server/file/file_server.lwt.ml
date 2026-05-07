open EzAPIServer
open File_server_arg

let () =
  Arg.parse specs conf "ezServe - Simple ez-api file server\nusage: ezserve [OPTIONS]";
  Lwt_main.run @@
  server ?addr:!addr [ !port, Root (!root, Some !default) ]
