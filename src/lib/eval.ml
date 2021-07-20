open Syntax
   
let lookup env v = try List.assoc v env with Not_found -> failwith ("Unbound var " ^ v)

let lookup_prim p = try List.assoc p Builtins.primitives with Not_found -> failwith ("Unbound primitive " ^ p)

let rec eval_expr env e =
  let open Expr in
  match e with
| EVar v -> lookup env v
| EInt c -> Value.Int c
| EPrim (p, es) ->
   let f = lookup_prim p in
   let args = List.map (eval_expr env) es in
   f args

let rec eval_state env states (name,args) =
  Printf.printf "Eval %s(%s) in env=[%s]\n"
    name
    (Misc.string_of_list Expr.to_string "," args)
    (Env.to_string env);
  let s = try List.assoc name states with Not_found -> failwith ("unbound state " ^ name) in
  let bindings = List.combine s.s_params (List.map (eval_expr env) args) in
  let env' = List.fold_left Env.update env bindings in
  let fireable (guard, _) = eval_expr env' guard = Value.Bool true in
  match List.find_opt fireable s.s_trans with
  | Some (_, Return e) -> eval_expr env' e
  | Some (_, Next (s',exprs')) -> eval_state env' states (s', exprs')
  | None -> failwith ("blocked in state " ^ name)

let eval_fsm_expr fsms (name, args) = 
  let f = try List.assoc name fsms with Not_found -> failwith ("Unbound fsm: " ^ name) in
  let env = List.combine f.f_params (List.map (eval_expr []) args) in
  match f.f_desc with
  | Let (states, (name, args)) ->
     eval_state env states (name, args)

let eval_program p = 
  List.map (eval_fsm_expr p.p_fsms) p.p_exprs

