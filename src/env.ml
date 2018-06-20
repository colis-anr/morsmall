
type t =
  { return_code : int ;
    under_condition : bool }

let default =
  { return_code = 0 ;
    under_condition = false }

let is_success env =
  ReturnCode.is_success env.return_code

let is_error env =
  ReturnCode.is_error env.return_code

let rc_to_int env =
  env.return_code
  
let negate env =
  { env with return_code = ReturnCode.negate env.return_code }
