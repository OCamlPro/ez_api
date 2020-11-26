
(* Use EzRequest for queries *)

(* Contrarily to EzXhr, EzCurl creates synchronous requests, i.e.
EzRequest.get and EzRequest.post returns only when the query has been
completed. If ~error is specified, no exception can occur, and ~error
will be called in case of error. If ~error is specified, an exception
is raised in case of error.
 *)

module Interface : EzRequest_lwt.Interface
include EzRequest_lwt.S
