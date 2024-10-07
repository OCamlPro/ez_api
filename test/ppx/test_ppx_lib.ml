
type error = {
  name: string;
  msg: string;
} [@@deriving encoding]

let%service errors = [
  EzAPI.Err.make ~code:400 ~name:"Error" ~encoding:error_enc ~select:Option.some ~deselect:Fun.id
]
and security : EzAPI.Security.bearer list = [ `Bearer {EzAPI.Security.bearer_name="Bearer"; format=None} ]

type nonrec test_derive_input = {
  foo: string;
  bar: int;
}
and test_derive_output = int
[@@post {path="/test/getter"}]

let%post echo_input = {
  path="/echo_input"; raw_input=["text/plain"];
  output=Json_encoding.(obj1 (req "test" string))
}
