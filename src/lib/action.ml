type t = 
  | Assign of string * Expr.t        (* var/i/o, value *)

let to_string a = match a with
  | Assign (id, expr) -> id ^ ":=" ^ Expr.to_string expr
