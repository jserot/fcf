type t =
  | Bool of bool
  | Int of int
  | Tuple of t list

let rec to_string v = match v with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Tuple vs -> "(" ^ Misc.string_of_list to_string "," vs ^ ")"
          
