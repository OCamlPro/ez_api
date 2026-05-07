(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type recaptcha = {
  cap_success: bool;
  cap_score: float option;
  cap_action: string option;
  cap_challenge_ts: string option;
  cap_hostname: string option;
  cap_error_codes: string list option
}

module Encoding = struct
  open Json_encoding

  let captcha =
    conv
      (fun {cap_success; cap_score; cap_action; cap_challenge_ts;
            cap_hostname; cap_error_codes}
        -> (cap_success, cap_score, cap_action, cap_challenge_ts,
            cap_hostname, cap_error_codes))
      (fun (cap_success, cap_score, cap_action, cap_challenge_ts,
            cap_hostname, cap_error_codes)
        -> {cap_success; cap_score; cap_action; cap_challenge_ts;
            cap_hostname; cap_error_codes})
      (obj6
         (req "success" bool)
         (opt "score" float)
         (opt "action" string)
         (opt "challenge_ts" string)
         (opt "hostname" string)
         (opt "error-codes" (list string)))
end

let recaptcha_url = "https://www.google.com/recaptcha/api/siteverify"

let verify secret_key token =
  let url = EzAPI.URL (
      recaptcha_url ^ "?secret=" ^ secret_key ^ "&response=" ^ token) in
  Lwt.map
    (function Error e -> Error e | Ok x -> Ok (EzEncoding.destruct Encoding.captcha x))
    (EzReq_lwt.post url)
