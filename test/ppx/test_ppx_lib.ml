
type error = {
  name: string;
  msg: string;
} [@@deriving encoding]

let%service errors = [
  EzAPI.Err.make ~code:400 ~name:"Error" ~encoding:error_enc ~select:Option.some ~deselect:Fun.id
]
and security : EzAPI.Security.bearer list = [ `Bearer {EzAPI.Security.bearer_name="Bearer"; format=None} ]
and base = "http://localhost:8080"

type test_derive = {
  foo: string;
  bar: int;
} [@@get {path="/test/getter"; debug}]

let%post echo_input = {
  path="/echo_input"; raw_input=["text/plain"];
  output=Json_encoding.(obj1 (req "test" string))
}
