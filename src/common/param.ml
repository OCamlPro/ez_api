type kind = PARAM_INT | PARAM_STRING | PARAM_BOOL

type t = {
  param_id : string;
  param_name : string option;
  param_descr : string option;
  param_type : kind;
  param_required : bool;
  param_examples : string list;
  param_schema : Json_schema.schema option;
}

let make param_type ?name ?descr ?(required=false) ?(examples=[]) ?schema param_id =
  { param_id; param_name = name; param_descr = descr;
    param_type; param_required = required; param_examples = examples;
    param_schema = schema }

let string = make PARAM_STRING

let int = make PARAM_INT

let bool = make PARAM_BOOL
