open Syntax
open Types

type tenv = {  (* TE *)
  te_cons: (string * int) list; (* constructor name, arity *)
  te_vars: (string * typ) list; (* type variables *)
  }

type venv = (string * typ_scheme) list (* VE *)

let no_loc = Location.no_location
           
exception Unbound_value of Location.location * string 
exception Wrong_type of string * typ * typ * Location.location
exception Circular_type of string * typ * typ * Location.location
exception Illegal_type_var of Location.location
exception Unknown_type_ctor of string * Location.location
exception Wrong_type_arity of string * int * int * Location.location
exception Wrong_guard_type of typ * Location.location

let lookup_value venv loc id = 
    try List.assoc id venv
    with Not_found -> raise (Unbound_value (loc,id))

(* let lookup_type tenv loc id =
 *     try List.assoc id tenv
 *     with Not_found -> unbound_type id loc *)

(* Typing programs *)

type typed_program = {
  tp_fsms: (string * typed_fsm) list;
  tp_insts: typ list;
  }

and typed_fsm = {
    tf_sig: typ_scheme;
    tf_states: (string * typ) list;
  }

(* Unification *)

let try_unify site ty1 ty2 loc =
  try
    Types.unify ty1 ty2
  with 
    TypeConflict(t1, t2) -> raise (Wrong_type (site,t1,t2,loc))
  | TypeCircularity(t1, t2) -> raise (Circular_type (site,t1,t2,loc))

(* Typing type expressions *)

let rec type_of_type_expression tenv te =
  let ty =
    match te.te_desc with
    | Typeconstr (c,args) ->
       check_type_arity c te.te_loc tenv args;
       TyConstr (c, List.map (type_of_type_expression tenv) args)
    | Typevar v ->
       raise (Illegal_type_var te.te_loc) in
  te.te_typ <- Types.real_type ty;
  ty

and check_type_arity c loc tenv args = 
  let arity =
    try List.assoc c tenv.te_cons
    with Not_found -> raise (Unknown_type_ctor (c,loc)) in
  let nargs = List.length args in
  if nargs <> arity then
    raise (Wrong_type_arity (c,arity,nargs,loc))

(* Typing expressions *)
  
let rec type_expression tenv venv expr =
  let ty = match expr.e_desc with
  | EVar id ->
     type_instance (lookup_value venv expr.e_loc id)
  | ETuple es ->
     type_product (List.map (type_expression tenv venv) es)
  | EInt _ ->  type_int
  | EBool _ ->  type_bool
  | EBinop (op, e1, e2) ->
     let ty_op = type_instance (lookup_value venv expr.e_loc op) in
     let ty_e1 = type_expression tenv venv e1 in
     let ty_e2 = type_expression tenv venv e2 in
     let ty_result = new_type_var () in
     try_unify "expression" ty_op (type_arrow (type_pair ty_e1 ty_e2) ty_result) expr.e_loc ;
     ty_result
  in
  expr.e_typ <- Types.real_type ty;
  ty

and type_application tenv venv { ap_desc=fn, args; ap_loc=loc } =
  let ty_fn = type_instance (lookup_value venv loc fn) in
  let ty_args = type_product (List.map (type_expression tenv venv) args) in
  let ty_result = new_type_var () in
  try_unify "application" ty_fn (type_arrow ty_args ty_result) loc;
  ty_result

(* Typing FSMs *)

let type_pattern id = 
  let ty = new_type_var () in
  ty, (id, trivial_scheme ty)

let type_state_continuation tenv venv cont = match cont.ct_desc with
  | Next e -> type_application tenv venv e
  | Return e -> type_expression tenv venv e

let type_state_trans tenv venv { t_desc = guard, cont; t_loc = loc } = 
  let ty = type_expression tenv venv guard in
  if is_bool_type ty
  then type_state_continuation tenv venv cont, loc
  else raise (Wrong_guard_type (ty, guard.e_loc))

let type_state_defn tenv venv sd =
  (* Type ( s(x_1,...,x_m)= | guard_1 -> cont_1 ... | guard_n -> cont_n ) = t_1 * ... * t_m -> t
     where 
     Type (guard_i) = bool for all i=1...n
     Type (cont_i) = t for all i=1...n *)
  let name, params, transitions = sd.sd_desc in 
  let ty_params, venv' = List.map type_pattern params |> List.split in
  let ty_transs = List.map (type_state_trans tenv (venv'@venv)) transitions in
  let ty_result = 
    begin match ty_transs with
  | [] -> Misc.fatal_error "Typing.type_state_defn" (* should not happen *)
  | (ty,_)::tys ->
     List.iter (fun (ty',loc) -> try_unify "state transition" ty ty' loc) tys;
     ty
    end in
  let ty = type_arrow (type_product ty_params) ty_result in
  sd.sd_params <- List.combine params ty_params;
  sd.sd_typ <- ty;
  ty
  
let type_state_defns tenv venv defns =
  (* Typing [let state_1(x_11,...)= a_1... and ... state_n(x_n1,...)=a_n]
     gives an environment [{ state_1: t_11*... -> t_1, ..., state_n: t_n1*... -> t_n }] *) 
  let ty_lhs, venv' =
    List.map
      (fun {sd_desc=name,_,_} -> type_pattern name)
      defns
    |> List.split in
  let ty_rhs =
    List.map
      (type_state_defn tenv (venv'@venv))
      defns in
  Misc.list_iter3
    (fun sd ty1 ty2 -> try_unify "state definition" ty1 ty2 sd.sd_loc)
    defns
    ty_lhs
    ty_rhs;
  let venv'' =
    List.map2 (fun (id,_) ty -> id, generalize venv ty) 
    venv' 
    ty_rhs in
  venv''

let type_fsm_decl tenv venv (name,d) =
  (* Typing [let fsm(x_1,...,x_m) = let <defns> in <appl>]
     gives type (scheme) [t_1 * ... * t_m -> t]
     where t = Type (appl) *)
  let f = d.fd_desc in
  let ty_params, venv' = List.map type_pattern f.f_params |> List.split in
  let defns, expr = f.f_desc in
  let venv'' = type_state_defns tenv (venv'@venv) defns in
  let ty_result = type_application tenv (venv''@venv'@venv) expr in
  let ty = generalize venv @@ type_arrow (type_product ty_params) ty_result in
  f.f_typ <- ty;
  name, ty

(* Typing programs *)
  
let type_program (tycons,venv) p = 
  let tenv = { te_cons = tycons; te_vars = [] } in
  let venv' = List.map (type_fsm_decl tenv venv) p.p_fsms in 
  { tp_fsms =
      List.map
        (fun (name, {fd_desc={f_typ=ty; f_desc=state_defns,_}}) ->
          name,
          { tf_sig = ty;
            tf_states =
              List.map
                (fun sd -> let name,_,_ = sd.sd_desc in name, sd.sd_typ)
                state_defns })
        p.p_fsms;
    tp_insts =
      List.map
        (type_application tenv (venv'@venv))
        p.p_insts }

(* Printing *)

let rec dump_typed_program tp =
  Printf.printf "Typed program ---------------\n";
  Printf.printf "- FSMs ----------------------\n";
  List.iter dump_typed_fsm tp.tp_fsms;
  Printf.printf "- Instances -----------------\n";
  List.iter dump_typed_inst tp.tp_insts;
  Printf.printf "-----------------------------\n"

and dump_typed_inst ty =
  Printf.printf "- : %s\n" (Pr_type.string_of_type ty);
  flush stdout

and dump_typed_fsm (name, f) =
  Pr_type.reset_type_var_names ();
  Printf.printf "fsm %s : %s\n" name (Pr_type.string_of_type_scheme f.tf_sig);
  List.iter (fun (id,ty) -> Printf.printf "  state %s: %s\n" id (Pr_type.string_of_type ty)) f.tf_states;
  flush stdout

(* let dump_typing_environment title (tenv,venv) = 
 *   Printf.printf "%s ---------------\n" title;
 *   List.iter dump_tycon tenv;
 *   List.iter dump_typed_value venv;
 *   Printf.printf "----------------------------------\n" *)
