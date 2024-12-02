
type error = {
  name: string;
  msg: string;
} [@@deriving encoding]

let%secu bearer = `Bearer {EzAPI.Security.bearer_name="Bearer"; format=None}

let%service errors = [
  EzAPI.Err.make ~code:400 ~name:"Error" ~encoding:error_enc ~select:Option.some ~deselect:Fun.id
]
and security : [ EzAPI.Security.none | EzAPI.Security.bearer ] list = [ bearer ]
and base = "http://localhost:8080"

type test_derive = {
  foo: string;
  bar: int;
} [@@get {path="/test/getter/{id: string}/{numb: int}"; name="bla"}]

type foo = [ `foo | `bar ] [@@deriving encoding {assoc}]

let%param foo : foo = {assoc; required}

let%post echo_input = {
  path="/echo_input"; raw_input=["text/plain"];
  output=Json_encoding.(obj1 (req "test" string));
  params=[foo]; security
}
