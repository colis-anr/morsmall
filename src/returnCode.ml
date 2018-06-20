
type t = int

let success = 0
let error = 1

let is_success rc =
  rc = 0

let is_error rc =
  rc <> 0

let negate rc =
  if is_success rc then
    error
  else
    success
