open Ffsm
open Expr
open Syntax
   
let fact = {
    f_name = "fact";
    f_params = ["n"];
    f_desc = 
      let acc = EVar "acc"
      and k = EVar "k"
      and n = EVar "n" in
      Let(
          [("compute",  (* Let ... *)
                  {s_params = ["acc"; "k"];
                   s_trans = [
                     EPrim ("<=", [k; n]), Next ("compute", [EPrim ("*",[acc; k]); EPrim ("+",[k; EConst 1])]);
                     EPrim (">", [k; n]), Return acc;
                     ]})],  (* ... in *)
          ("compute", [EConst 1; EConst 1]))
  }


let r = Eval.eval_program {p_fsm=fact; p_args=[EConst 3]}

let _ = Printf.printf "r=%s\n" (Value.to_string r)
