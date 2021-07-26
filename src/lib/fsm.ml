type t = {
  m_name: string;
  m_states: State.t list;
  m_inps: (string * Types.t) list;
  m_outps: (string * Types.t) list;
  m_vars: (string * Types.t) list;
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

let vars_of f = match f.f_desc with
  | state_defns, _ -> 
     let add acc params = 
       List.fold_left 
         (fun acc (id,ty) -> if List.mem_assoc id acc then acc else (id,ty)::acc)
         acc
         params in
     List.fold_left
       (fun acc sd -> add acc sd.sd_params)
       []
       state_defns 

let state_assignations senv s exprs = 
  let params,transitions = List.assoc s senv in
  List.map2 (fun (p,_) e -> Action.Assign (p, e)) params exprs

let strans_of f = match f.f_desc with
  | state_defns, { ap_desc = s,es } -> 
     let senv = state_env state_defns in
     "idle",
     mk_expr (EBinop ("=", mk_expr (EVar "start"), mk_expr (EInt 1))), 
     state_assignations senv  s es @ [Action.Assign ("rdy", mk_expr (EInt 0))],
     s

let mk_trans senv src { t_desc=g,k } = match k with
  | { ct_desc = Return e } -> 
     src, g, [Action.Assign ("res", e); Action.Assign ("rdy", mk_expr (EInt 1))], "idle"
  | { ct_desc = Next { ap_desc = dst, es} } ->
     src, g, state_assignations senv dst es, dst
     
let rtrans_of f = match f.f_desc with
  | state_defns, _ -> 
     let senv = state_env state_defns in
     List.fold_left
       (fun acc (s, (params,transitions)) -> acc @ List.map (mk_trans senv s) transitions)
       []
       senv

let from_ast f = {
  m_name = f.f_name;
  m_states = "idle" :: states_of f;
  m_inps =   ("start", Types.TyBool)
           :: List.map2
                (fun (p,_) ty -> p, ty)
                f.f_params
                (fst @@ Types.fn_types @@ Types.type_instance f.f_typ);
  m_outps = ("rdy", Types.TyBool) ::  [ "res", Types.type_int () ];
  m_vars = vars_of f;
  m_trans = strans_of f :: rtrans_of f;
  m_itrans = "idle", []
}
  
let string_of_typed_io (id, ty) = id ^ ":" ^ Types.string_of_type ty

let dump f = 
  let open Printf in 
  printf "FSM %s:\n" f.m_name;
  printf "  states = %s\n" @@ Misc.string_of_list Fun.id "," f.m_states;
  printf "  inps = %s\n" @@ Misc.string_of_list string_of_typed_io "," f.m_inps;
  printf "  outps = %s\n" @@ Misc.string_of_list string_of_typed_io "," f.m_outps;
  printf "  vars = %s\n" @@ Misc.string_of_list string_of_typed_io "," f.m_vars;
  printf "  trans =\n";
  List.iter (fun t -> printf "    %s\n" @@ Transition.to_string t) f.m_trans;
  printf "  itrans = ->%s\n" @@ fst f.m_itrans
