type t =
  | Bool of bool
  | Int of int

let to_string v = match v with Bool b -> string_of_bool b | Int i -> string_of_int i
          
