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
   Value.Array (Array.map (eval_expr env) es)
| EArrRd (a,e') -> 
   begin match
     lookup env e.e_loc a, eval_expr env e' with
     | Value.Array vs, Value.Int i -> 
        if i >= 0 && i < Array.length vs then Array.get vs i
        else raise (IllegalArrayAccess e)
     | _, _ ->
        Misc.fatal_error "Illegal array expression" (* should not happen *)
   end
| ECon0 c -> Con0 c
| ECon1 (c,e) -> Con1 (c, eval_expr env e)

let eval_action env { ac_desc = lhs, expr } =
  let v = eval_expr env expr in
  match lhs with
  | LVar id ->
     Env.update env (id,v)
  | LArr (id,idx) -> 
      begin match Env.lookup env id, eval_expr env idx with
       | Array a, Int i -> a.(i) <- v  (* in-place update *)
       | _ -> Misc.fatal_error "Eval.eval_action: illegal array access" (* should not happen *)
      end;
      env

exception Matching_fail

let rec eval_match v pat =
  let open Value in
  match (v, pat.p_desc) with
  | (v, Pat_var id) -> [id, v]
  | (Bool b1, Pat_bool b2) ->
      if b1 = b2 then [] else raise Matching_fail
  | (Int i1, Pat_int i2) ->
      if i1 = i2 then [] else raise Matching_fail
  | (Tuple vs, Pat_tuple ps) ->
      if List.length vs = List.length ps then
        List.flatten (List.map2 eval_match vs ps)
      else
        raise Matching_fail
  | (Con0 c1, Pat_constr0 c2) ->
      if c1 = c2 then [] else raise Matching_fail
  | (Con1 (c1, v1), Pat_constr1 (c2, p2)) ->
      if c1 = c2 then eval_match v1 p2 else raise Matching_fail
  | (_, _) -> raise Matching_fail
   
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
  let fireable ({ t_desc= guards, _, _ } as t) =
    let check_single_guard g = 
      (* Returns [(true, bindings)] is guard [g] (for transition [t]) is fireable *)
      match g.g_desc with
      | Cond expr ->
         eval_expr env' expr = Value.Bool true,
         []
      | Match (expr,pat) ->
         let v = eval_expr env' expr in
         begin
           try
             true,
             eval_match v pat
           with
             Matching_fail ->
             false,
             []
         end in
    (* guard [g1,...,gn] is fireable iff _each_ single guard [gi] gives [true];
       in this case, the set of the resulting bindings is the union of the collected bindings *)
    let (bs,bss) = List.map check_single_guard guards |> List.split in
    if List.for_all Fun.id bs then 
      true,
      (t, List.concat bss)
    else
      false,
      (t,[]) in
  match Misc.list_find_opt2 fireable transitions with
  | Some ({ t_desc= _, _, { ct_desc=Return e } }, bindings') ->
     let env'' = List.fold_left Env.update env' bindings' in
     eval_expr env'' e
  | Some ({ t_desc= _, acts, { ct_desc=Next { ap_desc = s',exprs' } } }, bindings') ->
     let env'' = List.fold_left Env.update env' bindings' in
     let env''' = List.fold_left eval_action env'' acts in
     eval_state env''' state_defns (s', exprs')
  | None ->
     raise (BlockedInState name)

let init_var env { v_desc=id,ty,iv } =
  match ty.te_desc, iv with
  | TeArray (sz,ty'), None -> id, Value.Array (Array.make sz Value.Unknown) (* special case *)
  | _, Some e -> id, eval_expr env e
  | _, None -> id, Value.Unknown

let eval_fsm_inst genv fsms { ap_desc=name,args } =
  let f =
    try List.assoc name fsms
    with Not_found -> Misc.fatal_error "Eval.eval_fsm_inst" (* should not happen thx to TC *) in
  let env_p = List.map2 (fun (id,_) arg -> id, eval_expr [] arg) f.f_params args in
  let env_v = List.map (init_var (env_p @ genv)) f.f_vars in 
  let lenv = env_p @ env_v in
  match f.f_desc with
  | state_defns, { ap_desc=name, args } ->
     eval_state (lenv @ genv) state_defns (name, args)

let fsm_env = List.map (function n, fd -> n, fd.fd_desc)
            
let eval_const_decl env (name,d) = 
  (name, eval_expr env d.cst_desc.c_val)

let eval_program p = 
  let global_env = List.map (eval_const_decl []) p.p_consts in
  List.map (eval_fsm_inst global_env (fsm_env p.p_fsms)) p.p_insts

