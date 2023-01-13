(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Map.Make(Int)

let codes =
  empty
  |> add 100 "Continue"
  |> add 101 "Switching Protocols"
  |> add 102 "Processing"
  |> add 103 "Early Hints"
  |> add 200 "OK"
  |> add 201 "Created"
  |> add 202 "Accepted"
  |> add 203 "Non-Authoritative Information"
  |> add 204 "No Content"
  |> add 205 "Reset Content"
  |> add 206 "Partial Content"
  |> add 207 "Multi-Status"
  |> add 208 "Already Reported"
  |> add 226 "IM Used"
  |> add 300 "Multiple Choices"
  |> add 301 "Moved Permanently"
  |> add 302 "Found"
  |> add 303 "See Other"
  |> add 304 "Not Modified"
  |> add 305 "Use Proxy"
  |> add 306 "Switch Proxy"
  |> add 307 "Temporary Redirect"
  |> add 308 "Permanent Redirect"
  |> add 400 "Bad Request"
  |> add 401 "Unauthorized"
  |> add 402 "Payment Required"
  |> add 403 "Forbidden"
  |> add 404 "Not Found"
  |> add 405 "Method Not Allowed"
  |> add 406 "Not Acceptable"
  |> add 407 "Proxy Authentication Required"
  |> add 408 "Request Timeout"
  |> add 409 "Conflict"
  |> add 410 "Gone"
  |> add 411 "Length Required"
  |> add 412 "Precondition Failed"
  |> add 413 "Payload Too Large"
  |> add 414 "URI Too Long"
  |> add 415 "Unsupported Media Type"
  |> add 416 "Range Not Satisfiable"
  |> add 417 "Expectation Failed"
  |> add 418 "I'm a teapot"
  |> add 421 "Misdirected Request"
  |> add 422 "Unprocessable Entity"
  |> add 423 "Locked"
  |> add 424 "Failed Dependency"
  |> add 425 "Too Early"
  |> add 426 "Upgrade Required"
  |> add 428 "Precondition Required"
  |> add 429 "Too Many Requests"
  |> add 431 "Request Header Fields Too Large"
  |> add 451 "Unavailable For Legal Reasons"
  |> add 500 "Internal Server Error"
  |> add 501 "Not Implemented"
  |> add 502 "Bad Gateway"
  |> add 503 "Service Unavailable"
  |> add 504 "Gateway Timeout"
  |> add 505 "HTTP Version Not Supported"
  |> add 506 "Variant Also Negotiates"
  |> add 507 "Insufficient Storage"
  |> add 508 "Loop Detected"
  |> add 510 "Not Extended"
  |> add 511 "Network Authentication Required"

let error code = find_opt code codes
let code s = fold (fun code err acc -> match acc with
    | None -> if err = s then Some code else None
    | Some c -> Some c) codes None
