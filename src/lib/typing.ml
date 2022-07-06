open Syntax
open Types

type tenv = {  
  te_types: (string * type_desc) list; (* type constructors (ex: "int", "list", ...) *)
  te_ctors: (string * constr_desc) list; (* (non builtin) value constructors (ex: "Nil", "Cons", ...) *)
  te_vars: (string * Types.t) list; (* type variables *)
  }

type venv = (string * typ_scheme) list

let no_loc = Location.no_location
           
exception Unbound_value of Location.location * string 
exception Wrong_type of string * Types.t * Types.t * Location.location
exception Circular_type of string * Types.t * Types.t * Location.location
exception Illegal_type_var of Location.location
exception Unknown_type_ctor of string * Location.location
exception Wrong_type_arity of string * int * int * Location.location
exception Unbound_type_ctor of string * Location.location
exception Unbound_value_ctor of string * Location.location
exception Ctor_arity_mismatch of string * int * int * Location.location
exception Duplicate_type_param of string * string * Location.location
exception Unbound_type_var of string * Location.location
exception Duplicate_param_in_type_decl_err of Location.location

let lookup_value venv loc id = 
    try List.assoc id venv
    with Not_found -> raise (Unbound_value (loc,id))

let lookup_type tenv loc id = 
  try List.assoc id tenv.te_types 
  with Not_found -> raise (Unbound_type_ctor (id,loc))

let lookup_ctor tenv loc id = 
  try List.assoc id tenv.te_ctors
  with Not_found -> raise (Unbound_value_ctor (id,loc))

let type_inst env t = type_instance (generalize env t)

(* Typing programs *)

type typed_program = {
  tp_types: (string * type_desc) list;
  tp_ctors: (string * constr_desc) list; 
  tp_consts: (string * Types.t) list;
  tp_fsms: (string * typed_fsm) list;
  tp_insts: (top_symbol list * string * typed_inst) list;
  }

and typed_fsm = {
    tf_sig: typ_scheme;
    tf_vars: (string * Types.t) list;
    tf_states: (string * Types.t) list;
  }

and typed_inst = {
  ti_args: Types.t list;
  ti_results: Types.t list;
  }

(* Unification *)

let try_unify site ty1 ty2 loc =
  try
    Types.unify ty1 ty2
  with 
    TypeConflict(t1, t2) -> raise (Wrong_type (site,t1,t2,loc))
  | TypeCircularity(t1, t2) -> raise (Circular_type (site,t1,t2,loc))

(* Type coercion *)

exception Illegal_type_coercion of Types.t * Types.t * Location.location

let try_cast ty1 ty2 loc = match real_type ty1, real_type ty2 with
  | TyInt (_,_), TyInt(_,_) -> () (* All ints are coercable. To be refined *)
  | _, _ -> raise (Illegal_type_coercion (ty1,ty2,loc)) (* ... and nothing else *)

(* Keeping track of polymorphic type constructor instanciations *)
                             
let specialize_cdesc tvbs c =
  let bs = { tb_typ=tvbs; tb_sign=[]; tb_size=[] } in
  { c with
    (* cs_params = [];    (\* Specialisation turns a polymorphic value constr into a monomorphic one *\) *)
    cs_arg = Types.copy_type bs c.cs_arg;
    cs_res = Types.copy_type bs c.cs_res }

let specialize_type_defn tvbs (name,td) = match td with
    Variant_type (tvs,cds) ->
     TyCon (name, List.map snd tvbs),
     Variant_type ([], List.map (specialize_cdesc tvbs) cds)
    (* Specialisation turns a polymorphic type constr into a monomorphic one *)
| _ -> failwith "Typing.specialize_type_defn"    (* should not be happen *)

let update_tc_insts tenv loc ty = 
  let add_type_inst td (t,tc) =
    match List.find_opt (fun (t',_) -> type_equal t t') td.ty_insts with
    | None -> td.ty_insts <- (t,tc) :: td.ty_insts
    | _ -> () in
  match real_type ty with
    TyCon (name, ty_args) ->
      let td = lookup_type tenv loc name in
      begin match td.ty_desc, td.ty_arity, ty_args with
      | Variant_type ([], cds), 0, _ -> ()
      | Variant_type (tvs, cds), n, ty_args when List.for_all is_ground_type ty_args ->
            begin
              let tvbs = List.combine tvs (List.map real_type ty_args) in
              let t,tc = specialize_type_defn tvbs (name,td.ty_desc) in
              add_type_inst td (t,tc)
            end
      | _ -> () (* should not happen *)
      end
  | _ -> ()

(* Typing type expressions *)

let rec type_of_type_expression tenv te =
  let sign_attr sg = match sg with
    | None -> Var (make_var ())
    | Some TeSigned -> Const Signed
    | Some TeUnsigned -> Const Unsigned in
  let size_attr sz = match sz with
    | None -> Var (make_var ())
    | Some (TeWidth w) -> Const (Width w) 
    | Some (TeRange (lo,hi)) -> Const (Range (lo,hi)) in
  let ty = match te.te_desc with
    | TeInt (sg, sz) -> TyInt (sign_attr sg, size_attr sz)
    | TeBool -> TyBool
    | TeFloat -> TyFloat
    | TeArray (sz,te') -> type_sized_array sz (type_of_type_expression tenv te')
    | TeConstr (c, tes) ->
       let td = lookup_type tenv te.te_loc c in
       if td.ty_arity <> List.length tes then 
         raise (Ctor_arity_mismatch (c, td.ty_arity, List.length tes, te.te_loc))
       else
        type_constr c (List.map (type_of_type_expression tenv) tes)
    | TeVar v ->
       begin
         try List.assoc v tenv.te_vars
         with Not_found -> raise (Unbound_type_var (v, te.te_loc))
       end
  in
  te.te_typ <- Types.real_type ty;
  ty

(* Typing expressions *)
  
let rec type_expression tenv venv expr =
  let ty = match expr.e_desc with
  | EVar id ->
     type_instance (lookup_value venv expr.e_loc id)
  | ETuple es ->
     TyProduct (List.map (type_expression tenv venv) es)
  | EInt _ ->  type_int ()
  | EBool _ ->  TyBool
  | EFloat _ ->  TyFloat
  | EBinop (op, e1, e2) ->
     let ty_op = type_instance (lookup_value venv expr.e_loc op) in
     let ty_e1 = type_expression tenv venv e1 in
     let ty_e2 = type_expression tenv venv e2 in
     let ty_result = TyVar (Types.new_type_var ()) in
     try_unify "expression" ty_op (type_arrow (type_pair ty_e1 ty_e2) ty_result) expr.e_loc ;
     ty_result
  | EArray [||] ->
     Misc.fatal_error "empty array" (* syntactically forbidden *)
  | EArray es ->
     let ty_elem = type_expression tenv venv es.(0) in
     for i=1 to Array.length es-1 do
       let e' = es.(i) in
       try_unify "array element" (type_expression tenv venv e') ty_elem e'.e_loc
     done;
     type_sized_array (Array.length es) ty_elem
  | EArrRd (a,i) ->
     let ty_arr = type_instance (lookup_value venv expr.e_loc a) in
     let ty_idx = type_expression tenv venv i in
     let ty_result = TyVar (Types.new_type_var ()) in
     try_unify "array index" ty_idx (type_int ()) expr.e_loc;
     try_unify "expression" ty_arr (type_array ty_result) expr.e_loc;
     ty_result
  | ECon0 c ->
     let cd = lookup_ctor tenv expr.e_loc c in
     if cd.cs_arity = 0
     then type_inst venv cd.cs_res
     (* else type_copy (type_arrow cdsc.cs_arg cdsc.cs_res) *)
     else raise (Ctor_arity_mismatch (cd.cs_name, cd.cs_arity, 0, expr.e_loc))
  | ECon1 (c,e) ->
     let cd = lookup_ctor tenv expr.e_loc c in
     (* if cd.cs_arity <> List.length es then 
      *   raise (Ctor_arity_mismatch (cd.cs_name, cd.cs_arity, List.length es, expr.e_loc))
      * else *)
       let ty_arg = type_inst venv cd.cs_arg in
       let ty_res = type_inst venv cd.cs_res in
       try_unify "expression" ty_arg (type_expression tenv venv e) expr.e_loc;
       ty_res
  | ECast (e,t) ->
     let t1 = type_expression tenv venv e in
     let t2 = type_of_type_expression tenv t in
     try_cast t1 t2 expr.e_loc;
     t2
  in
  expr.e_typ <- Types.real_type ty;
  update_tc_insts tenv expr.e_loc ty;
  ty

and type_application tenv venv { ap_desc=fn, args; ap_loc=loc } =
  let ty_fn = type_instance (lookup_value venv loc fn) in
  let ty_args = TyProduct (List.map (type_expression tenv venv) args) in
  let ty_result = TyVar (Types.new_type_var ()) in
  try_unify "application" ty_fn (type_arrow ty_args ty_result) loc;
  ty_args, ty_result

(* Typing TYPE decls *)

let bind_type_params name loc params =
  List.fold_left
    (fun acc v -> 
      if List.mem_assoc v acc then
        raise (Duplicate_type_param (name, v, loc))
      else 
        (v, Types.new_type_var ()) :: acc)
    []
    params

let type_type_decl tenv (name, {td_desc=params,td; td_loc=loc}) =
  let arity = List.length params in
  let tvars = bind_type_params name loc params in
  let ty_vars = List.map (fun (n,v) -> n, TyVar v) tvars in 
  let ty_res = type_constr name (List.map snd ty_vars) in
  let type_comp, ctors =
    match td with
    | Variant_decl constrs ->
        let cds =
          List.map
            (function
              | Constr0_decl id ->
                  id,
                  { cs_name=id; cs_arity=0; (* cs_params=[]; *) cs_res=ty_res; cs_arg=type_unit } 
              | Constr1_decl (id, args) ->
                 let defined_type = { ty_arity = arity; ty_desc = Abstract_type; ty_insts = [] } in (* Temporary *)
                 let tenv' = { tenv with te_types = (name, defined_type) :: tenv.te_types; te_vars = ty_vars } in
                 let arg, arity = 
                   match List.map (type_of_type_expression tenv') args with
                   | [t] -> t, 1
                   | ts -> TyProduct ts, List.length ts in
                 id,
                 { cs_name = id; cs_arity = arity; (* cs_params=[]; *) cs_arg = arg; cs_res = ty_res })
            constrs in
        Variant_type (List.map snd tvars, List.map snd cds), cds in
  { tenv with te_types = tenv.te_types @ [name, { ty_arity = arity; ty_desc = type_comp; ty_insts = [] }];
              te_ctors = tenv.te_ctors @ ctors }

(* Typing CONST decls *)

exception Illegal_const_type of string * Types.t
exception Illegal_poly_type of Types.t

let type_const_decl tenv venv (name,d) =
  let c = d.cst_desc in
  let ty = type_expression tenv venv c.c_val in
  if Types.is_const_type ty then begin
    let ty' = type_of_type_expression tenv c.c_typ in
    try_unify "array declaration" ty ty' d.cst_loc;
    name, ty
    end
  else
    raise (Illegal_const_type (name, ty))

(* Typing FSMs *)

let type_param tenv (id,t) = 
  let ty = match t with
    | None -> TyVar (new_type_var ())
    | Some te -> type_of_type_expression tenv te in
  ty, (id, trivial_scheme ty)

let type_var tenv venv ({ v_desc=id,t,iv; v_loc=loc } as v) = 
  let ty = type_of_type_expression tenv t in
  let ty' = match iv with 
    | Some e -> 
       let ty' = type_expression tenv venv e in
       try_unify "local variable declaration" ty ty' loc;
       ty'
    | None -> 
       ty in
  v.v_typ <- ty';
  id, trivial_scheme ty'

let type_var_decl tenv (id,t) = 
  id, type_of_type_expression tenv t 

let type_state_pattern id = 
  let ty = TyVar (new_type_var ()) in
  ty, (id, trivial_scheme ty)

let rec type_pattern tenv env p =
  let ty, bindings = match p.p_desc with
  | Pat_var id ->
     let ty = TyVar (new_type_var ()) in
      ty, (id, trivial_scheme ty) :: env
  | Pat_bool b ->
      (TyBool, env)
  | Pat_int n ->
      (type_int (), env)
  | Pat_tuple ps ->
      let env', tys =
        List.fold_left 
          (fun (env,ts) p -> let t, env' = type_pattern tenv env p in (env', t::ts))
          (env, [])
          ps in
      (type_product (List.rev tys), env')
  | Pat_constr0 c ->
     let cd = lookup_ctor tenv p.p_loc c in
     if cd.cs_arity = 0
     then type_inst [] cd.cs_res, env
     else raise (Ctor_arity_mismatch (cd.cs_name, cd.cs_arity, 0, p.p_loc))
  | Pat_constr1 (c, arg) ->
     let cd = lookup_ctor tenv p.p_loc c in
     let ty_arg, env' = type_pattern tenv env arg in
     let ty_res = TyVar (new_type_var ()) in
     let t = type_inst [] (type_arrow cd.cs_arg cd.cs_res) in
     try_unify "pattern" t (type_arrow ty_arg ty_res) p.p_loc;
     ty_res, env' in
  p.p_typ <- Types.real_type ty;
  ty, bindings

let type_match loc tenv venv expr pat =
  let ty_p, venv' = type_pattern tenv venv pat in
  let ty_e = type_expression tenv venv expr in
  try_unify "match clause" ty_e ty_p loc;
  venv'
  
let type_state_continuation tenv venv cont = match cont.ct_desc with
  | Next e -> snd @@ type_application tenv venv e
  | Return e -> type_expression tenv venv e

let type_state_guard tenv venv g = 
  match g.g_desc with 
  | Cond expr ->
     let ty_g = type_expression tenv venv expr in
     try_unify "guard expression" ty_g TyBool g.g_loc;
     []
  | Match (expr,pat) ->
     let venv' = type_match g.g_loc tenv venv expr pat in
     venv'

let type_lhs tenv venv loc lhs = match lhs with
  | LVar v ->
     type_instance (lookup_value venv loc v)
  | LArr (a,i) ->
     let ty_arr = type_instance (lookup_value venv loc a) in
     let ty_idx = type_expression tenv venv i in
     try_unify "array index" ty_idx (type_int ()) loc;
     let ty_result = TyVar (Types.new_type_var ()) in
     try_unify "expression" ty_arr (type_array ty_result) loc;
     ty_result

let type_assignation tenv venv { ac_desc=lhs, exp; ac_loc=loc } =
  let ty_l = type_lhs tenv venv loc lhs in
  let ty_r = type_expression tenv venv exp in
  try_unify "assignation" ty_l ty_r loc

let type_state_trans tenv venv { t_desc = guards, acts, cont; t_loc = loc } = 
  let venv' = List.flatten @@ List.map (type_state_guard tenv venv) guards in
  List.iter (type_assignation tenv venv) acts;
  type_state_continuation tenv (venv'@venv) cont, loc

let type_state_defn tenv venv sd =
  (* Type ( s(x_1,...,x_m)= | guard_1 -> cont_1 ... | guard_n -> cont_n ) = t_1 * ... * t_m -> t
     where 
     Type (guard_i) = bool for all i=1...n
     Type (cont_i) = t for all i=1...n *)
  let name, params, transitions = sd.sd_desc in 
  let ty_params, venv' = List.map (type_param tenv) params |> List.split in
  let ty_transs = List.map (type_state_trans tenv (venv'@venv)) transitions in
  let ty_result = 
    begin match ty_transs with
  | [] -> Misc.fatal_error "Typing.type_state_defn" (* should not happen *)
  | (ty,_)::tys ->
     List.iter (fun (ty',loc) -> try_unify "state transition" ty ty' loc) tys;
     ty
    end in
  let ty = Types.real_type @@ type_arrow (TyProduct ty_params) ty_result in
  sd.sd_params <- List.map2 (fun (id,_) ty -> id, ty) params ty_params;
  sd.sd_typ <- ty;
  ty
  
let type_state_defns tenv venv defns =
  (* Typing [let state_1(x_11,...)= a_1... and ... state_n(x_n1,...)=a_n]
     gives an environment [{ state_1: t_11*... -> t_1, ..., state_n: t_n1*... -> t_n }] *) 
  let ty_lhs, venv' =
    List.map
      (fun {sd_desc=name,_,_} -> type_state_pattern name)
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
  let ty_params, venv_p = List.map (type_param tenv) f.f_params |> List.split in
  let venv_v = List.map (type_var tenv (venv_p@venv)) f.f_vars in
  let defns, expr = f.f_desc in
  let venv_s = type_state_defns tenv (venv_p@venv_v@venv) defns in
  let ty_result = snd @@ type_application tenv (venv_s@venv_p@venv_v@venv) expr in
  let ty = generalize venv @@ type_arrow (TyProduct ty_params) ty_result in
  f.f_typ <- ty;
  name, ty

(* Typing FSM instances *)

let type_fsm_inst tenv venv (lhs, ({ap_desc = f, args} as appl)) = 
  let ty_args, ty_res = type_application tenv venv appl in 
  begin match lhs, Types.real_type ty_res with
    | [l], ty ->
       l.top_typ <- ty
    | ls, TyProduct ts when List.length ls = List.length ts ->
       List.iter2 (fun l ty -> l.top_typ <- ty) ls ts
    | _, ty_rhs ->
       let ty_lhs = TyVar (Types.new_type_var ()) in
       raise (Wrong_type ("toplevel binding",ty_lhs,ty_rhs,appl.ap_loc))
  end;
  let venv' = List.map (fun l -> l.top_id, Types.trivial_scheme l.top_typ) lhs in
  let typed_inst = lhs, f, { ti_args = Types.list_of_types ty_args; ti_results = Types.list_of_types ty_res } in
  venv @ venv', typed_inst
  
(* Typing programs *)
  
let type_program (builtin_tenv,builtin_venv) p = 
  let tenv0 = {
    te_types = List.map (fun (name,arity) -> name, { ty_arity=arity; ty_desc=Abstract_type; ty_insts=[] }) builtin_tenv;
    te_ctors = [];
    te_vars = [] } in
  let tenv = List.fold_left type_type_decl tenv0 p.p_types in
  (* let tenv = { te_cons = tycons; te_vars = [] } in *)
  let venv = builtin_venv in
  let ty_consts = List.map (type_const_decl tenv venv) p.p_consts in 
  let venv_c = List.map (fun (id,ty) -> id, generalize venv ty) ty_consts in
  let venv_f = List.map (type_fsm_decl tenv (venv_c @ venv)) p.p_fsms in 
  { tp_types = tenv.te_types;
    tp_ctors = tenv.te_ctors;
    tp_consts = ty_consts;
    tp_fsms =
      List.map
        (fun (name, {fd_desc={f_typ=ty; f_vars=vars; f_desc=state_defns,_}}) ->
          name,
          { tf_sig = ty;
            tf_vars = List.map (fun { v_desc=id,te,_ } -> id, type_of_type_expression tenv te) vars;
            tf_states =
              List.map
                (fun sd -> let name,_,_ = sd.sd_desc in name, sd.sd_typ)
                state_defns })
        p.p_fsms;
    tp_insts =
      let genv = venv_c @ venv_f @ venv in
      let _, res = List.fold_left_map
        (type_fsm_inst tenv)
        genv
        p.p_insts in
      res }

(* Printing *)

let rec dump_typed_program tp =
  Printf.printf "Typed program ---------------\n";
  Printf.printf "- TYPES ---------------------\n";
  List.iter dump_typed_type tp.tp_types;
  (* Printf.printf "- CTORS ---------------------\n";
   * List.iter dump_typed_ctor tp.tp_ctors; *)
  Printf.printf "- CONSTs --------------------\n";
  List.iter dump_typed_const tp.tp_consts;
  Printf.printf "- FSMs ----------------------\n";
  List.iter dump_typed_fsm tp.tp_fsms;
  Printf.printf "- Instances -----------------\n";
  List.iter dump_typed_inst tp.tp_insts;
  Printf.printf "-----------------------------\n"

and dump_typed_type (name,td) =
  Printf.printf "%s : %s\n" name (Types.string_of_type_desc td);
  flush stdout

and dump_typed_ctor (name,cd) =
  Printf.printf "%s : %s\n" name (Types.string_of_ctor_desc cd);
  flush stdout

and dump_typed_const (name,ty) =
  Printf.printf "%s : %s\n" name (Types.string_of_type ty);
  flush stdout

and dump_typed_inst (lhs, fsm, ti) =
  let string_of_types tys = Misc.string_of_list Types.string_of_type "*" tys in
  Printf.printf "%s : %s\n"
    (Misc.string_of_list (fun s -> s.top_id) ", " lhs)
    (* (string_of_types ti.ti_args) *)
    (string_of_types ti.ti_results);
  flush stdout

and dump_typed_fsm (name, f) =
  Printf.printf "fsm %s : %s\n" name (Types.string_of_type_scheme f.tf_sig);
  List.iter (fun (id,ty) -> Printf.printf "  var %s: %s\n" id (Types.string_of_type ty)) f.tf_vars;
  List.iter (fun (id,ty) -> Printf.printf "  state %s: %s\n" id (Types.string_of_type ty)) f.tf_states;
  flush stdout

let rec dump_typing_environment venv = 
  Printf.printf "Typing environment ---------------\n";
  List.iter dump_typed_value venv;
  Printf.printf "----------------------------------\n"

and dump_typed_value (name, ts) =
  Printf.printf "val %s : %s\n" name (Types.string_of_type_scheme ts);
  flush stdout

