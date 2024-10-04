type error = .. [@@deriving err_case]

type error +=
  | NotFound of string [@code 404]
  | Unauthorized of string [@code 401]
  | Error1
  | Error2
[@@deriving err_case {code=400}]

let%err_case generic_case : error EzAPI.Err.case = 400
