
let () =
  EzCurl.init (); (* Use Curl implementation for Xhr *)
  (* EzXhr.init ();  (* Use Javascript implementation for Xhr *) *)
  Test.main ()
