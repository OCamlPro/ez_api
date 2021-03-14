let echo (_, s) () =
  EzAPIServer.return_ok @@ "echo arg: " ^ s
[@@get {path="/echo/{arg : string}"; raw_output=[]; debug}]

let echo_input s =
  EzAPIServer.return_ok @@ "echo input: " ^ s
[@@service Test_ppx_lib.echo_input]

[@@@server 8080]
