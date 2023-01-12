open EzAPI

let api_port = 8884

let web_host = "http://localhost:8885"

let api_host = "http://localhost:" ^ string_of_int api_port

module Types = struct
  type input = {user: string; hash: string}

  type output = {name: string; query: string; version: int}

  type user_info = string
end

module Encoding = struct
  open Types
  open Json_encoding

  let input =
    conv
      (fun {user; hash} -> (user, hash))
      (fun (user, hash) -> {user; hash})
      (obj2 (req "user" string) (req "hash" string))

  let output =
    conv
      (fun {name; query; version} -> (name, query, version))
      (fun (name, query, version) -> {name; query; version})
      (obj3 (req "name" string) (req "query" string) (req "version" int))

  let user_info = string
end

module SessionArg = struct
  type user_id = string

  type user_info = Types.user_info

  let user_id_encoding = Json_encoding.string

  let user_info_encoding = Encoding.user_info

  let web_host = Some web_host

  let rpc_path = []

  let token_kind = `Cookie ("Session_id", None)
end

let user1_login = "user1"

let user1_password = "123"

let user1_info = "info-on-user"

module Services = struct
  let security =
    Security.
      [`Cookie ({ref_name= "Session id cookie"; name= "Session_id"}, None)]

  let access_control = [
    "access-control-allow-origin", web_host;
    "access-control-allow-credentials", "true" 
  ]

  let arg_test = Arg.string ~example:"example-of-value" "arg-in-path"

  let param_arg =
    Param.string ~name:"arg-string" ~descr:"An example argument" "arg"

  let section_test = Doc.section "Tests Requests"

  let test1 : (Types.output, exn, no_security) service0 =
    service ~section:section_test ~name:"test1" ~params:[param_arg]
      ~output:Encoding.output ~access_control
      Path.(root // "test1")

  let test11 : (Types.output, exn, no_security) service0 =
    service ~section:section_test ~name:"test11" ~meth:`POST
      ~params:[param_arg] ~output:Encoding.output ~access_control
      Path.(root // "test1")

  let test2 : (string, Types.output, exn, no_security) service1 =
    service ~section:section_test ~name:"test2" ~params:[param_arg]
      ~output:Encoding.output ~access_control
      Path.(root // "test2" /: arg_test)

  let test22 : (string, Types.output, exn, no_security) service1 =
    service ~section:section_test ~name:"test2" ~meth:`POST
      ~params:[param_arg] ~output:Encoding.output ~access_control
      Path.(root // "test2" /: arg_test)

  let test3 : (Types.input, Types.output, exn, no_security) post_service0 =
    post_service ~section:section_test ~name:"test3" ~params:[] ~access_control
      ~input:Encoding.input ~output:Encoding.output
      Path.(root // "test3")

  let test4 :
      ( string
      , Types.input
      , Types.output
      , exn
      , EzAPI.Security.cookie )
      post_service1 =
    post_service ~section:section_test ~name:"test4" ~params:[param_arg]
      ~input:Encoding.input ~output:Encoding.output ~security ~access_control
      Path.(root // "test4" /: arg_test)
end
