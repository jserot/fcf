open Syntax
open Location

let trace = ref false

exception Unbound_value of location * string 

let lookup env loc id =
  try List.assoc id env
  with Not_found -> raise (Unbound_value (loc,id))
   
let rec eval_expr env e =
  match e.e_desc with
| EVar v -> lookup env e.e_loc v
| EInt c -> Value.Int c
| EPrim (p, es) ->
   let f = lookup Builtins.primitives e.e_loc p in
   let args = List.map (eval_expr env) es in
   f args

let rec eval_state env state_decls (name,args) =
  if !trace then 
    Printf.printf "Eval %s(%s) in env=[%s]\n"
      name
      (Misc.string_of_list string_of_expr "," args)
      (Env.to_string env);
  let s =
    try List.assoc name (List.map (fun sd -> sd.sd_desc) state_decls)
    with Not_found -> failwith ("unbound state " ^ name) in
  let bindings = List.combine s.sd_params (List.map (eval_expr env) args) in
  let env' = List.fold_left Env.update env bindings in
  let fireable { t_desc= guard, _ } = eval_expr env' guard = Value.Bool true in
  match List.find_opt fireable s.sd_trans with
  | Some { t_desc= _, { ct_desc=Return e } } -> eval_expr env' e
  | Some { t_desc= _, { ct_desc=Next (s',exprs') } } -> eval_state env' state_decls (s', exprs')
  | None -> failwith ("blocked in state " ^ name)

let eval_fsm_inst fsms { fi_desc=name, args } =
  let f = try List.assoc name fsms with Not_found -> failwith ("Unbound fsm: " ^ name) in
  let env = List.combine f.f_params (List.map (eval_expr []) args) in
  match f.f_desc with
  | state_decls, (name, args) ->
     eval_state env state_decls (name, args)

let fsm_env = List.map (function n, fd -> n, fd.fd_desc)
            
let eval_program p = 
  List.map (eval_fsm_inst (fsm_env p.p_fsms)) p.p_insts

