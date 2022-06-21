type t = 
  | Assign of Syntax.lhs * Syntax.expr

let rename_vars f a = match a with
  | Assign (lhs, e) -> Assign (Syntax.rename_lhs_vars f lhs, Syntax.rename_expr_vars f e)

let to_string a = match a with
  | Assign (lhs, expr) -> Syntax.string_of_lhs lhs ^ ":=" ^ Syntax.string_of_expr expr

