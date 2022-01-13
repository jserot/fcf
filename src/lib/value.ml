type t =
  | Bool of bool
  | Int of int
  | Float of float
  | Tuple of t list
  | Array of t list

let rec to_string v = match v with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Tuple vs -> "(" ^ Misc.string_of_list to_string "," vs ^ ")"
  | Array vs -> "{" ^ Misc.string_of_list ~max_elems:(!Syntax.array_print_length) to_string "," vs ^ "}"
          
