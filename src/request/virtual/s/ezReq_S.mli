(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzAPI

type error_handler = (int -> string option -> unit)

module type CURRENT = sig

  val get0 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string -> (* debug msg *)
    ?error: error_handler ->            (* unhandled error handler *)
    base_url ->                   (* API url *)
    ('output, 'error, [< Security.scheme ]) service0 -> (* GET service *)
    (('output, 'error) result -> unit) -> (* reply handler *)
    unit

  val get1 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg: string ->
    ?error: error_handler ->
    base_url ->
    ('arg, 'output, 'error, [< Security.scheme ]) service1 ->
    'arg ->
    (('output, 'error) result -> unit) ->
    unit

  val get2 :
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?error: error_handler ->
    base_url ->
    ('arg1, 'arg2, 'output, 'error, [< Security.scheme ]) service2 ->
    'arg1 -> 'arg2 ->
    (('output, 'error) result -> unit) ->
    unit

  val post0 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    ?error: error_handler ->          (* error handler *)
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('input, 'output, 'error, [< Security.scheme ]) post_service0 -> (* POST service *)
    (('output, 'error) result -> unit) -> (* reply handler *)
    unit

  val post1 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    ?error: error_handler ->          (* error handler *)
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('arg, 'input, 'output, 'error, [< Security.scheme ]) post_service1 -> (* POST service *)
    'arg ->
    (('output, 'error) result -> unit) -> (* reply handler *)
    unit

  val post2 :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?url_encode:bool ->
    ?error: error_handler ->          (* error handler *)
    input:'input ->                           (* input *)
    base_url ->                 (* API url *)
    ('arg1, 'arg2, 'input, 'output, 'error, [< Security.scheme ]) post_service2 -> (* POST service *)
    'arg1 -> 'arg2 ->
    (('output, 'error) result -> unit) -> (* reply handler *)
    unit

  val get :
    ?meth:Meth.all ->
    ?headers:(string * string) list ->
    ?msg:string ->                 (* debug msg *)
    ?error:error_handler ->   (* error handler *)
    url ->              (* url *)
    (string -> unit) ->       (* normal handler *)
    unit

  val post :
    ?meth:Meth.all ->
    ?content_type:string ->
    ?content:string ->
    ?headers:(string * string) list ->
    ?msg:string ->
    ?error:error_handler ->
    url ->
    (string -> unit) -> unit

  val request :
    ?headers:(string * string) list ->
    ?params:(Param.t * param_value) list ->
    ?msg:string ->
    ?post:bool ->
    ?url_encode:bool ->
    ?error: error_handler ->
    input:'input ->
    base_url ->
    ('args, 'input, 'output, 'error, [< Security.scheme ]) service ->
    'args ->
    (('output, 'error) result -> unit) ->
    unit
end


module type LEGACY = sig

  val get0 :
    base_url ->                   (* API url *)
    'output Legacy.service0 -> (* GET service *)
    string ->                           (* debug msg *)
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->            (* unhandled error handler *)
    ?params:(Param.t * param_value) list ->
    ('output -> unit) -> (* reply handler *)
    unit ->                           (* trigger *)
    unit

  val get1 :
    base_url ->
    ('arg, 'output) Legacy.service1 ->
    string ->
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->
    ?params:(Param.t * param_value) list ->
    ('output -> unit) ->
    'arg ->
    unit

  val get2 :
    base_url ->
    ('arg1, 'arg2, 'output) Legacy.service2 ->
    string ->
    ?post:bool ->
    ?headers:(string * string) list ->
    ?error: error_handler ->
    ?params:(Param.t * param_value) list ->
    ('output -> unit) ->
    'arg1 -> 'arg2 ->
    unit

  val post0 :
    base_url ->                 (* API url *)
    ('input, 'output) Legacy.post_service0 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(Param.t * param_value) list ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    ('output -> unit) -> (* reply handler *)
    unit

  val post1 :
    base_url ->                 (* API url *)
    ('arg, 'input, 'output) Legacy.post_service1 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(Param.t * param_value) list ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    'arg ->
    ('output -> unit) -> (* reply handler *)
    unit

  val post2 :
    base_url ->                 (* API url *)
    ('arg1, 'arg2, 'input, 'output) Legacy.post_service2 -> (* POST service *)
    string ->                         (* debug msg *)
    ?headers:(string * string) list ->
    ?error: error_handler ->          (* error handler *)
    ?params:(Param.t * param_value) list ->
    ?url_encode:bool ->
    input:'input ->                           (* input *)
    'arg1 -> 'arg2 ->
    ('output -> unit) -> (* reply handler *)
    unit

  val get :
    ?meth:Meth.all ->
    string ->                 (* debug msg *)
    url ->              (* url *)
    ?headers:(string * string) list ->
    ?error:error_handler ->   (* error handler *)
    (string ->
     unit) ->       (* normal handler *)
    unit

  val post :
    ?meth:Meth.all ->
    ?content_type:string ->
    ?content:string ->
    string ->
    url ->
    ?headers:(string * string) list ->
    ?error:error_handler ->
    (string -> unit) -> unit

end

(* This interface is exported by all engines, so that you can directly
use them from there. *)
module type S = sig

  include CURRENT

  module Legacy : LEGACY

  val init : unit -> unit

  (* hook executed before every request *)
  val add_hook : (unit -> unit) -> unit
  (* hook executed after every request *)
  val add_reply_hook : (unit -> unit) -> unit

end

module type Interface = sig
  val get :
    ?meth:string ->
    ?headers:(string * string) list ->
    ?msg:string -> string ->
    ((string, int * string option) result -> unit) -> unit

  val post :
    ?meth:string ->
    ?content_type:string ->
    ?content:string ->
    ?headers:(string * string) list ->
    ?msg:string -> string ->
    ((string, int * string option) result -> unit) -> unit
end
