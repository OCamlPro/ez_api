type nonrec test_derive_input = {
  foo: string;
  bar: int;
}
and test_derive_output = int
[@@post {path="/test/getter"; debug}]

let%post echo_input = {
  path="/echo_input"; raw_input=["text/plain"];
  output=Json_encoding.(obj1 (req "test" string))
}
