type t =
  | Bool of bool
  | Int of int
  | Float of float
  | Tuple of t list
  | Array of t array
  | Con0 of string
  | Con1 of string * t 
  | Unknown (* for uninitialized local variables *)

let rec to_string v = match v with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Tuple vs -> "(" ^ Misc.string_of_list to_string "," vs ^ ")"
  | Array vs -> "{" ^ Misc.string_of_array ~max_elems:(!Syntax.array_print_length) to_string "," vs ^ "}"
  | Con0 c -> c
  | Con1 (c,v) -> c ^ " " ^ to_string v
  | Unknown -> "?"
          
