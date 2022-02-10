open Syntax

let trace = ref false
let max_array_print = ref 4

exception BlockedInState of string 
exception IllegalArrayAccess of expr 

let lookup env loc id =
  try List.assoc id env
  with Not_found -> raise (Typing.Unbound_value (loc,id)) (* should not happen after TC *)
   
let rec eval_expr env e =
  match e.e_desc with
| EVar v -> lookup env e.e_loc v
| EInt c -> Value.Int c
| EBool c -> Value.Bool c
| EFloat c -> Value.Float c
| ETuple es -> Tuple (List.map (eval_expr env) es)
| EBinop (op, e1, e2) ->
   let _, f = lookup Builtins.primitives e.e_loc op in
   let args = List.map (eval_expr env) [e1; e2] in
   f args
| EArray es ->
   Value.Array (List.map (eval_expr env) es)
| EArrRd (a,e') -> 
   begin match
     lookup env e.e_loc a, eval_expr env e' with
     | Value.Array vs, Value.Int i -> 
        if i >= 0 && i < List.length vs then Array.get (Array.of_list vs) i
        else raise (IllegalArrayAccess e)
     | _, _ ->
        Misc.fatal_error "Illegal array expression" (* should not happen *)
   end
| ECon0 c -> Con0 c
| ECon1 (c,e) -> Con1 (c, eval_expr env e)
   
let rec eval_state env state_defns (name,args) =
  if !trace then 
    Printf.printf "Eval %s(%s) in env=[%s]\n"
      name
      (Misc.string_of_list string_of_expr "," args)
      (Env.to_string env);
  let lookup_state s = 
    let rec lookup defns = match defns with
      | [] -> Misc.fatal_error "Eval.eval_state" (* should not happen thx to TC *)
      | {sd_desc=name,params,trans}::rest -> if s=name then params, trans else lookup rest in
    lookup state_defns in
  let params, transitions = lookup_state name in
  let bindings = List.map2 (fun (id,_) arg -> id, eval_expr env arg) params args in
  let env' = List.fold_left Env.update env bindings in
  let fireable { t_desc= guard, _ } =
    match guard.g_desc with
    | Cond expr -> eval_expr env' expr = Value.Bool true
    | Match _ -> failwith "** Eval.eval_state: not implemented: match-guard" in
  match List.find_opt fireable transitions with
  | Some { t_desc= _, { ct_desc=Return e } } -> eval_expr env' e
  | Some { t_desc= _, { ct_desc=Next { ap_desc = s',exprs' } } } -> eval_state env' state_defns (s', exprs')
  | None -> raise (BlockedInState name)

let eval_fsm_inst genv fsms { ap_desc=name,args } =
  let f =
    try List.assoc name fsms
    with Not_found -> Misc.fatal_error "Eval.eval_fsm_inst" (* should not happen thx to TC *) in
  let lenv = List.map2 (fun (id,_) arg -> id, eval_expr [] arg) f.f_params args in
  match f.f_desc with
  | state_defns, { ap_desc=name, args } ->
     eval_state (lenv @ genv) state_defns (name, args)

let fsm_env = List.map (function n, fd -> n, fd.fd_desc)
            
let eval_const_decl env (name,d) = 
  (name, eval_expr env d.cst_desc.c_val)

let eval_program p = 
  let global_env = List.map (eval_const_decl []) p.p_consts in
  List.map (eval_fsm_inst global_env (fsm_env p.p_fsms)) p.p_insts

