let%post echo_input = {
  path="/echo_input"; raw_input=["text/plain"];
  output=Json_encoding.(obj1 (req "test" string))
}
