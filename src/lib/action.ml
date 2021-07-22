type t = 
  | Assign of string * Syntax.expr        (* var/i/o, value *)

let to_string a = match a with
  | Assign (id, expr) -> id ^ ":=" ^ Syntax.string_of_expr expr
