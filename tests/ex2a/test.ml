open Ffsm
open Expr
open Syntax
   
let is_even = {
    f_name = "is_even";
    f_params = ["n"];
    f_desc = 
      Let(
          [("even", 
                  {s_params = ["m"];
                   s_trans = [
                     EPrim (">", [EVar "m"; EConst 0]), Next ("odd", [EPrim ("-",[EVar "m"; EConst 1])]);
                     EPrim ("<=", [EVar "m"; EConst 0]), Return (EConst 1);
           ]});
           ("odd", 
                  {s_params = ["m"];
                   s_trans = [
                     EPrim (">", [EVar "m"; EConst 0]), Next ("even", [EPrim ("-",[EVar "m"; EConst 1])]);
                     EPrim ("<=", [EVar "m"; EConst 0]), Return (EConst 0)]})
          ],
          ("even", [EVar "n"]))
  }


let r = Eval.eval_program {p_fsm=is_even; p_args=[EConst 3]}
let _ = Printf.printf "r=%s\n" (Value.to_string r)

let r = Eval.eval_program {p_fsm=is_even; p_args=[EConst 2]}
let _ = Printf.printf "r=%s\n" (Value.to_string r)
