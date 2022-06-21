type cfg = {
  res_id: string;
  }

let cfg = {
  res_id = "res";
  }

type t = {
  m_name: string;
  m_states: State.t list;
  m_inps: (string * Types.t) list;
  m_outps: (string * Types.t) list;
  m_vars: (string * (Types.t * Syntax.expr option)) list;
  m_trans: Transition.t list;
  m_itrans: State.t * Action.t list;
  }

open Syntax

let state_env state_defns =
  List.map
    (fun sd -> let name,params,trans = sd.sd_desc in name, (params,trans))
    state_defns

let states_of f = match f.f_desc with
  | state_defns, _ -> List.map fst @@ state_env state_defns

let rename_state_vars f = 
  let rename_state_defn ({ sd_desc=name,params,transitions; sd_params=params' } as sd) =
    let rename id = if List.mem_assoc id params then name ^ "_" ^ id else id in
    { sd with sd_desc = (name,
                        List.map (fun (id,te) -> rename id, te) params,
                        List.map (Syntax.rename_trans_vars rename) transitions);
              sd_params = List.map (fun (id,t) -> rename id, t) params' } in
  let states, appl = f.f_desc in 
  { f with f_desc = List.map rename_state_defn states, appl }
  
let vars_of_states fd = match fd with
  | state_defns, _ -> 
     let add acc params = 
       List.fold_left 
         (fun acc (id,ty) -> if List.mem_assoc id acc then acc else (id,(ty,None))::acc)
         acc
         params in
     List.fold_left
       (fun acc sd -> add acc sd.sd_params)
       []
       state_defns 

let state_assignations senv s exprs = 
  let params,transitions = List.assoc s senv in
  List.map2 (fun (p,_) e -> Action.Assign (Syntax.LVar p, e)) params exprs

let var_initialisations vars =
  List.fold_left 
    (fun acc { v_desc=id,_,iv; v_typ=ty } ->
        match iv with 
        | None -> acc
        | Some e -> Action.Assign (LVar id, e) :: acc)
    []
    vars

let strans_of f = match f.f_desc with
  | state_defns, { ap_desc = s,es } -> 
     let senv = state_env state_defns in
     "idle",
     [mk_cond_guard @@ mk_bool_expr @@ EBinop ("=", mk_bool_expr @@ EVar "start", mk_bool_expr @@ EBool true)], 
       state_assignations senv s es
     @ var_initialisations f.f_vars 
     @ [Action.Assign (Syntax.LVar "rdy", mk_bool_expr @@ EBool false)],
     s

let mk_trans senv src { t_desc=g,acts,k } =
  let explicit_actions =
    List.map 
      (fun { ac_desc=lhs,expr } -> Action.Assign (lhs, expr))
      acts in
  match k with
  | { ct_desc = Return e } -> 
     src,
     g,
     explicit_actions @ [Action.Assign (Syntax.LVar cfg.res_id, e); Action.Assign (Syntax.LVar "rdy", mk_bool_expr @@ EBool true)],
     "idle"
  | { ct_desc = Next {ap_desc=dst,es} } ->
     src,
     g,
     explicit_actions @ state_assignations senv dst es,
     dst
     
let rtrans_of f = match f.f_desc with
  | state_defns, _ -> 
     let senv = state_env state_defns in
     List.fold_left
       (fun acc (s, (params,transitions)) -> acc @ List.map (mk_trans senv s) transitions)
       []
       senv

let from_ast ?(rename_svars=false) f' =
  let f = if rename_svars then rename_state_vars f' else f' in 
  let ty_args, ty_res = Types.fn_types @@ Types.type_instance f.f_typ in
  { m_name = f.f_name;
    m_states = "idle" :: states_of f;
    m_inps =
        List.map2 (fun (p,_) ty -> p, ty) f.f_params ty_args
      @ ["start", Types.TyBool];
    m_outps = 
        (match ty_res with 
         | [] -> []
         | [t] -> [cfg.res_id, t]
         | ts -> List.mapi (fun i t -> cfg.res_id ^ string_of_int (i+1), t) ts)
      @ ["rdy", Types.TyBool];
    m_vars =
        List.map (fun { v_desc=id,_,iv; v_typ=ty } -> (id,(ty,iv))) f.f_vars
      @ vars_of_states f.f_desc;
    m_trans = strans_of f :: rtrans_of f;
    m_itrans = "idle", []
  }
  
let string_of_typed_io ?(tab="") (id,ty) = tab ^ id ^ ":" ^ Types.string_of_type ty

let string_of_typed_var ?(tab="") (id,(ty,iv)) = match iv with
  | None -> tab ^ id ^ ":" ^ Types.string_of_type ty
  | Some e -> tab ^ id ^ ":" ^ Types.string_of_type ty ^ "=" ^ string_of_expr e

let dump f = 
  let open Printf in 
  printf "FSM %s:\n" f.m_name;
  printf "  states = %s\n" @@ Misc.string_of_list Fun.id "," f.m_states;
  printf "  inps = %s\n" @@ Misc.string_of_list (string_of_typed_io ~tab:"") ", " f.m_inps;
  printf "  outps = %s\n" @@ Misc.string_of_list (string_of_typed_io ~tab:"") ", " f.m_outps;
  printf "  vars = %s\n" @@ Misc.string_of_list (string_of_typed_var ~tab:"") ", " f.m_vars;
  printf "  trans =\n";
  List.iter (fun t -> printf "    %s\n" @@ Transition.to_string t) f.m_trans;
  printf "  itrans = ->%s\n" @@ fst f.m_itrans
