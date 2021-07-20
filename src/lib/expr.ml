type t = 
| EVar of string
| EInt of int
| EPrim of string * t list

let rec to_string e = match e with
  | EVar v -> v
  | EInt c -> string_of_int c
  | EPrim (p, es) -> p ^ "(" ^ Misc.string_of_list to_string "," es ^ ")"

