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

let state_env state_decls = List.map (fun sd -> sd.sd_desc) state_decls

let states_of f = match f.f_desc with
  | state_decls, _ -> List.map fst (state_env state_decls)

let inps_of f = List.map Typing.no_type f.f_params

let outps_of f = [ Typing.no_type "res" ]

module VarSet = Set.Make (struct type t = string let compare = compare end)

let vars_of f = match f.f_desc with
  | state_decls, _ -> 
     List.fold_left
       (fun acc { sd_desc=_,sd } -> VarSet.union acc (VarSet.of_list sd.sd_params))
       VarSet.empty
       state_decls 
     |> VarSet.elements
     |> List.map Typing.no_type
  
let state_assignations senv s exprs = 
  let sd = List.assoc s senv in
  List.map2 (fun v e -> Action.Assign (v, e)) sd.sd_params exprs

let strans_of f = match f.f_desc with
  | state_decls, (s,es) -> 
     "idle",
     mk_expr (EPrim ("=", [mk_expr (EVar "start"); mk_expr (EInt 1)])), 
     state_assignations (state_env state_decls) s es @ [Action.Assign ("rdy", mk_expr (EInt 0))],
     s

let mk_trans senv src { t_desc=g,k } = match k with
  | { ct_desc = Return e } -> 
     src, g, [Action.Assign ("res", e); Action.Assign ("rdy", mk_expr (EInt 1))], "idle"
  | { ct_desc = Next (dst, es) } ->
     src, g,  state_assignations senv dst es, dst
     
let rtrans_of f = match f.f_desc with
  | state_decls, _ -> 
     let senv = state_env state_decls in
     List.fold_left
       (fun acc (s, sd) -> acc @ List.map (mk_trans senv s) sd.sd_trans)
       []
       senv

let from_ast f = {
  m_name = f.f_name;
  m_states = "idle" :: states_of f;
  m_inps = ("start", Types.NoType) :: inps_of f;
  m_outps = ("rdy", Types.NoType) :: outps_of f;
  m_vars = vars_of f;
  m_trans = strans_of f :: rtrans_of f;
  m_itrans = "idle", []
}
  
let string_of_io (id, _ ) = id

let dump f = 
  let open Printf in 
  printf "FSM %s:\n" f.m_name;
  printf "  states = %s\n" @@ Misc.string_of_list Fun.id "," f.m_states;
  printf "  inps = %s\n" @@ Misc.string_of_list string_of_io "," f.m_inps;
  printf "  outps = %s\n" @@ Misc.string_of_list string_of_io "," f.m_outps;
  printf "  vars = %s\n" @@ Misc.string_of_list string_of_io "," f.m_vars;
  printf "  trans =\n";
  List.iter (fun t -> printf "    %s\n" @@ Transition.to_string t) f.m_trans;
  printf "  itrans = ->%s\n" @@ fst f.m_itrans
