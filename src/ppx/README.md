## Client

To derive services for a client you can use the `ez_api.ppx_client` deriver. If you also want to derive caller functions you can use `ez_api.ppx_req`.

### Parameter

Service parameters can be defined via PPX, deriving useful construction and destruction functions that can be used among other things in the server handlers with the `[%req]` extension.
The parameter PPX can be used as:
```ocaml
let%param my_param : string = {
  id: string; (* parameter key in the url (default here: my_param) *)
  name: string; (* parameter name in the documentation (default here: my_param) *)
  descr: string; (* description in the documentation *)
  examples: string; (* examples in the documentation *)
  schema: Json_schema.schema; (* schema in the documentation, useful for string format *)
  required (or req): flag (* to ensure the parameter is required *)
  kind: EzAPI.Param.kind;
  int: flag; (* ensure the value is destruct as a int when using your own types or encoding *)
  bool: flag; (* idem *)
  obj: flag; (* idem *)
  enc: Json_encoding.encoding; (* use the encoding de destruct/construct the value *)
  destruct (or des): (string -> _ option) (* destruct function *)
  construct (or cons): (_ -> string) (* construct function *)
  assoc: flag; (* use the association list for example from enumeration [ `foo | `bar ] [@@deriving encoding {assoc}] *)

let%paramater my_second_param = () (* is also accepted *)
}
```
All options are of course optional.
The type use as constraint can help define the construct and destruct function. For int, int32, int64, nativeint, nativeint, float, bool and string it will be automatic; in other cases the functions will try to use an encoding defined as `<type_name>_enc`.

### Security

As for the parameters, the PPX helps building functions to check the header of the request.
The security PPX can be used as:
```ocaml
let%secu bearer_sec = `Bearer { bearer_name = ...; format = ... }
let%secu sec_schemes : EzAPI.Security.scheme list = [ bearer_sec ]
let%security ... = ... (* can also be used *)
```
The expression can be a security scheme or a security scheme list (which is the input in a service definition).

### Service

Services can be derived using:
```ocaml
let%get my_get_service = {
  path : string ; (* path of the service "/my/path/{my_arg : string}": arguments can be defined beforehand from EzAPI.Arg module *)
  security : EzAPI.Security.scheme list; (* security schemes usable for this service *)
  params : EzAPI.Param.t list; (* list of parameter for this service *)
  output : _ Json_encoding.encoding or list of mime types under the string format; (* output encoding *)
  input : _ Json_encoding.encoding or list of mime types under the string format; (* mostly for post/put/patch methods *)
  section : EzAPI.Doc.section; (* section for the documentation of the service *)
  name: string; (* name of the service, by default the pattern of the binding *)
  descr: string; (* description for the documentation *)
  hide: bool; (* hide from the documentation *)
  input_example: Json_repr.view; (* json for an input example *)
  output_example: Json_repr.view; (* json for an output example *)
  errors: _ EzAPI.Err.case list; (* list of error case *)
}
```

The services can also be derived from types. If you use 1, the encoding will be used as output for `get`, `put` and `delete` methods and as input for `post` and `patch` methods.
You can of course use 2 types when needed.
```ocaml
type input = {
  ...
}
and output = {
  ...
} [@@deriving encoding, post { path="/my/path"; name="my_service"; ... }]
```

### Globals

If you use `ez_api.ppx_req`, you can set some globals for the base of the path or for the headers:
```ocaml
[%%service {
  base: string or ref; (* default base for the callers *)
  headers: expression; (* default headers for the callers *)
  errors: EzAPI.Err.case list; (* default errors for the services *)
  security: EzAPI.Security.scheme list; (* default security for the services *)
}]
```

## Server

### Handler

From a handler, you can derive a service and link the service to the handler using a parameter.
```ocaml
let my_handler _req _security _input =
  ...
[@@get { path="..."; ... }]
```
If you have already defined the service elsewhere, you can link it:
```ocaml
let my_handler _req _security _input =
  ...
[@@service my_service]
```
The `_req` and `_security` arguments can be omitted.

To facilitate and you can use the argument `[%req]` to parse the parameters and security headers of the service defined with `%security` and `%params` extension.
```ocaml
let my_handler [%req] _security _input =
  let p = req#my_param in (* value option unless the param is required *)
  let token = req#bearer in (* string option for the token *) in
  ...
[@@service my_service]
```

### Globals

You can define a global wrapper and a global default request error to be used in the handlers:
```ocaml
let%handler req_error (e: string) = Error (BadRequest e)
and wrapper p = Lwt.bind p (fun x ->
  Result.iter_error print_error x;
  EzAPIServer.return x)
```
