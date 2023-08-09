let echo (_, s) () =
  EzAPIServer.return_ok @@ "echo arg: " ^ s
[@@get {path="/echo/{arg : string}"; raw_output=[]}]

let echo_input s =
  EzAPIServer.return_ok @@ "echo input: " ^ s
[@@service {service=Test_ppx_lib.echo_input} ]

let rec react s =
  Lwt.return_ok @@ "ws echo: " ^ s
and bg r sec send =
  send (Ok "Hey!");
  Lwt.bind (EzLwtSys.sleep 30.) (fun () -> bg r sec send)
and onclose _ =
  Lwt.return_unit
[@@websocket {path="/ws"; input=Json_encoding.string; output=Json_encoding.string}]

[%%server 8080]
