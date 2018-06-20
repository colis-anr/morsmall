
type t

val default : t
   
val is_success : t -> bool
val is_error : t -> bool
val negate : t -> t
val rc_to_int : t -> int
