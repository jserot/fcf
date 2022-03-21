type t = 
  | Assign of string * Syntax.expr        (* var/i/o, value *)

let rename_vars f a = match a with
  | Assign (id, e) -> Assign (f id, Syntax.rename_expr_vars f e)

let to_string a = match a with
  | Assign (id, expr) -> id ^ ":=" ^ Syntax.string_of_expr expr
