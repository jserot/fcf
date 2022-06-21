open Fcf
open Printf

type config = {
  mutable state_var: string;
  mutable reset_sig: string;
  mutable reset_duration: int;
  mutable clock_sig: string;
  mutable clock_period: int;
  mutable start_duration: int;
  mutable sim_interval: int;
  mutable time_unit: string;
  mutable act_sem: act_semantics;
  mutable dump_cc_intf: bool;
  mutable support_library: string;
  mutable support_packages: string list;
  mutable types_pkg_name: string;
  mutable consts_pkg_name: string;
  mutable with_testbench: bool;
  mutable tb_name: string;
  mutable int_size: int;
  mutable float_size: int;
  mutable heap_size: int;
  mutable heap_init_state: string;
  mutable trace_heap: bool;
  mutable print_heap_size: bool;
  mutable target: target_kind;
  mutable top: string
  }

and target_kind =
  | Flat
  | Quartus of string (* target dir *)
  | Sopc of string (* target dir *)

and act_semantics =  (** Interpretation of actions associated to transitions *)
  | Sequential        (** sequential (ex: [x:=x+1,y:=x] with [x=1] gives [x=2,y=2]) *)
  | Synchronous       (** synchronous (ex: [x:=x+1,y=x] with [x=1] gives [x=2,y=1]) *)

  
  
exception Error of string * string  (* where, msg *)
exception Duplicate_pattern of string * Fsm.t  (* when a pattern variable is used several times with different types across guards *)
exception Illegal_pattern of Syntax.pattern 
exception Heap_full of string (* where *)

let cfg = {
  state_var = "state";
  reset_sig = "rst";
  reset_duration = 2;
  clock_sig = "clk";
  clock_period = 10;
  start_duration = 10;
  sim_interval = 50;
  time_unit = "ns";
  act_sem = Synchronous;  (* Default *)
  dump_cc_intf = false;
  support_library = "fcf";
  support_packages = ["fcf.utils"; "fcf.values"];
  types_pkg_name = "types";
  consts_pkg_name = "consts";
  with_testbench = false;
  tb_name = "tb";
  int_size = 32;
  float_size = 32;
  heap_size = 16;  (* TO FIX *)
  heap_init_state = "InitH";
  trace_heap = false;
  print_heap_size = false;
  target = Flat;
  top = ""
  }

let set_quartus_target d =
  cfg.target <- Quartus d;
  cfg.support_library <- "work";
  cfg.support_packages <- ["work.utils"; "work.values"]

let set_sopc_target d =
  cfg.target <- Sopc d
  (* TO FIX ? directories and package names *)

open Vhdl_types

(* Models *)
  
type model = {
  v_name: string;
  v_states: string list;
  v_inps: (string * Types.t) list;
  v_outps: (string * Types.t) list;
  v_vars: (string * (Types.t * Syntax.expr option)) list;  (* "Regular" variables, with optional init value *)
  v_bvars: (string * Types.t) list; (* Variables bound in the guards of state transitions *) 
  v_init: State.t * Action.t list;
  v_trans: (State.t * Transition.t list) list; (* Transitions, here indexed by source state *)
  v_has_heap: bool;
  }

module StateSet = Set.Make (struct type t = string let compare = compare end)

let state_id = String.capitalize_ascii

let lookup_type tp id = 
  let open Typing in
  try List.assoc id tp.tp_types 
  with Not_found -> failwith "Vhdl.lookup_type" (* should not happen *)

let lookup_ctor tp id = 
  let open Typing in
  try List.assoc id tp.tp_ctors
  with Not_found -> failwith "Vhdl.lookup_ctor" (* should not happen *)

let add_heap_init_signals f = 
  let hstate = cfg.heap_init_state in
  let heap_size = cfg.heap_size in 
  let name s = f.Fsm.m_name ^ "_" ^ s in
  let open Fsm in
  let open Syntax in
  let istate = fst f.m_itrans in 
  { f with
    m_states = f.m_states @ [hstate]; 
    m_inps = f.m_inps
             @ ["h_init", Types.TyBool; "h_icnt", Types.TyAdhoc ("integer range 0 to " ^ string_of_int (heap_size-1));
                "h_ival", Types.TyAdhoc "block_t"];
    m_outps = f.m_outps
             @ ["h_heap", Types.TyAdhoc (name "heap_t"); "h_hptr", Types.TyAdhoc (name "hptr_t") ];
    m_vars = f.m_vars
             @ ["heap", (Types.TyAdhoc (name "heap_t"), None);
                "h_ptr", (Types.TyAdhoc (name "hptr_t"), None)];
    m_trans = f.m_trans (* TOFIX : also need to add L/C=0 conds to trans starting from istate *)
              @ [ istate, [mk_binop_guard "=" (EVar "h_init") (EBool true)],
                          [Assign (LVar "rdy", mk_bool_expr (EBool false));
                           Assign (LVar "h_ptr", mk_expr (EInt 0))], hstate;
                  hstate, [mk_binop_guard "<" (EVar "h_ptr") (EVar "h_icnt")],
                          [Assign (LVar "heap(h_ptr)", mk_expr (EVar "h_ival"));
                           Assign (LVar "h_ptr", mk_expr (EBinop ("+", mk_expr(EVar "h_ptr"), mk_expr (EInt 1))))], hstate;
                  hstate, [mk_binop_guard "=" (EVar "h_ptr") (EVar "h_icnt")], [Assign (LVar "rdy", mk_bool_expr (EBool true))], istate];
   }

let build_model f = 
  let open Fsm in
  let collect_bvs acc (_,guards,_,_) = 
    let open Syntax in 
    let rec collect_guard acc g  = match g.g_desc with 
      | Cond _ -> acc
      | Match (exp,pat) -> collect_pat acc pat
    and collect_pat acc p = match p.p_desc with
      | Pat_var v ->
         begin match List.assoc_opt v acc with
         | Some t ->
            if Types.type_equal t p.p_typ then acc
            else raise (Duplicate_pattern (v,f))
         | None -> (v,p.p_typ)::acc
         end
      | Pat_constr1 (_, p) ->
         collect_pat acc p
      | Pat_tuple ps ->
         List.fold_left collect_pat acc ps
      |  _ -> acc in
    List.fold_left collect_guard acc guards in
  let bvars = List.fold_left collect_bvs [] f.m_trans in
  let has_heap =
       List.exists (fun (_,t) -> Types.is_variant_type t) (f.m_inps @ f.m_outps @ bvars) 
    || List.exists (fun (_,(t,_)) -> Types.is_variant_type t) f.m_vars in
    (* Currently, FSMs needing a heap are only those manipulating variant types. WARNING: this may change.. *)
  let f' = if has_heap then add_heap_init_signals f else f in
  let mk_trans has_heap istate s = 
    let ts = List.filter (fun (s',_,_,_) -> s=s') f'.m_trans in
    state_id s,
    List.map
      (fun (s,guard,acts,s') ->
        state_id s,
        guard,
        acts,
        state_id s')
      ts in
  let src_states = 
    List.fold_left 
      (fun acc (src,_,_,_) -> StateSet.add src acc)
      StateSet.empty
      f'.m_trans
    |> StateSet.elements in
  let istate = state_id @@ fst f'.m_itrans in
  { v_name = f'.m_name;
    v_states = List.map state_id f'.m_states; 
    v_inps = f'.m_inps;
    v_outps = f'.m_outps;
    v_vars = f'.m_vars;
    v_bvars = bvars;
    v_init = istate, snd f'.m_itrans;
    v_trans = List.map (mk_trans has_heap istate) src_states;
    v_has_heap = has_heap;
    }

let extract_ud_types tp acc (name,td) =
  let open Types in
  match td.ty_desc with
  | Variant_type (_,_) -> acc @ List.map (mk_variant_type_desc (* tp *)) td.ty_insts
  | _ -> acc

let string_of_op op =
  let undot op =
    let l = String.length op in
    if String.get op (l-1) = '.' then String.sub op 0 (l-1) else op in
  match undot op with
  | "!=" -> " /= "
  | op ->  op

let rec string_of_expr e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let string_of_float x =
    let s = Stdlib.string_of_float x in
    if String.get s (String.length s -1) = '.' then s ^ "0" else s  in
  let rec string_of level e =
    match e.Syntax.e_desc, vhdl_type_of (e.Syntax.e_typ)  with
    | Syntax.EInt n, Unsigned s -> Printf.sprintf "to_unsigned(%d,%d)" n s
    | Syntax.EInt n, Signed s -> Printf.sprintf "to_signed(%d,%d)" n s
    | Syntax.EInt n, _ -> string_of_int n
    | Syntax.EBool b, _ -> if b then "'1'" else "'0'"
    | Syntax.EFloat n, _ -> string_of_float n
    | Syntax.EVar n, _ ->  n
    | Syntax.EBinop (">>",e1,e2), ty -> string_of_shift level "shift_right" e1 e2 (* Special cases *)
    | Syntax.EBinop ("<<",e1,e2), ty -> string_of_shift level "shift_left" e1 e2
    | Syntax.EBinop (op,e1,e2), ty -> 
       let s1 = string_of (level+1) e1 
       and s2 = string_of (level+1) e2 in 
       begin match op, ty with
       | "*", Signed _
       | "*", Unsigned _ -> "mul(" ^ s1 ^ "," ^ s2 ^ ")"
       | "&&", _ -> s1 ^ " and " ^ s2
       | "||", _ -> s1 ^ " or " ^ s2
       | _, _ -> paren level (s1 ^ string_of_op op ^ s2)
       end
    | Syntax.EArray es, _ -> Printf.sprintf "(%s)" (Misc.string_of_array string_of_expr "," es)
    | Syntax.EArrRd (a,idx), _ -> Printf.sprintf "%s(%s)" a (string_of level idx)
    (* | Syntax.ECon0 c, Variant vd -> Printf.sprintf "%s_mk_%s(heap,h_ptr,false)" vd.vd_name c
     * | Syntax.ECon1 (c,e'), Variant vd -> Printf.sprintf "%s_mk_%s(heap,h_ptr,%s)" vd.vd_name c (string_of_expr e')  *)
    | Syntax.ETuple es, _ -> Misc.string_of_list string_of_expr "," es
    | _ -> Misc.fatal_error "Vhdl.string_of_expr"
  and string_of_shift level op e1 e2 =
    op ^ "(" ^ string_of (level+1) e1 ^ "," ^ string_of_int_expr (level+1) e2 ^ ")"
  and string_of_int_expr level e = match vhdl_type_of e.Syntax.e_typ with 
      | Integer _ -> string_of level e
      | Unsigned _ | Signed _ -> "to_integer(" ^ string_of level e ^ ")"
      | _ -> Misc.fatal_error "Vhdl.string_of_expr"  (* should not happen *)
  in
  string_of 0 e

let string_of_lhs lhs = match lhs with
| Syntax.LVar v -> v
| Syntax.LArr (a,i) -> a ^ "(" ^ string_of_expr i ^ ")"

let string_of_action m a =
  let asn lhs = " <= " in
  let string_of_act lhs expr = string_of_lhs lhs ^ asn lhs ^ string_of_expr expr in
  match a with
  | Action.Assign (lhs, expr) ->
     begin match lhs, expr.e_desc, vhdl_type_of (expr.e_typ) with
     | LVar id, ETuple es, _ when id = Fsm.cfg.Fsm.res_id -> 
        Misc.string_of_list
          Fun.id
          "; " 
          (List.mapi (fun i e -> string_of_act (Syntax.LVar (id ^ string_of_int (i+1))) e) es)
     | LVar id, ECon0 c, Variant vd ->
        Printf.sprintf "%s_mk_%s(heap,h_ptr,false,%s)" vd.vd_name c id
     | LVar id, ECon1 (c,e'), Variant vd -> 
        Printf.sprintf "%s_mk_%s(heap,h_ptr,heap_size,%s,%s)" vd.vd_name c (string_of_expr e') id
     | _, _, _ ->
        string_of_act lhs expr
     end

let string_of_binding (var,expr) = var ^ " := " ^ string_of_expr expr

let scan_match expr pat = 
  let open Syntax in 
  let mk_expr e = { e_desc = e; e_loc = Location.no_location; e_typ = expr.e_typ } in
  match pat.p_desc, expr.e_typ with 
    | Pat_int v, _ -> string_of_expr (mk_expr (Syntax.EBinop("=", expr, mk_expr (EInt v)))), []
    | Pat_bool v, _ -> string_of_expr (mk_expr (Syntax.EBinop("=", expr, mk_expr (EBool v)))), []
    | Pat_var v, t -> "true", [v,0,expr,pat,t]
    | Pat_constr0 c, t ->
       Printf.sprintf "%s_match_%s(heap, %s)" (string_of_type t) c (string_of_expr expr),
       []
    | Pat_constr1 (c, {p_desc=Pat_int v}), t ->
       Printf.sprintf "%s_match_%s(heap, %s,%s,%s)"
         (string_of_type t)
         c
         (string_of_expr expr)
         "\"1\"" (* do not bind, just match constant [v] *)
         (string_of_int v),
       []
    | Pat_constr1 (c, {p_desc=Pat_var v; p_typ=t'}), t ->
       Printf.sprintf "%s_match_%s(heap,%s,%s,%s)"
         (string_of_type t)
         c
         (string_of_expr expr)
         "\"0\"" (* do not match, just bind *)
         (default_value @@ vhdl_type_of t'), (* must give a value, even if unused here *)
       [v,1,expr,pat,t] (* resultant binding *)
    | Pat_constr1 (c, {p_desc=Pat_tuple ps}), t ->
       let string_of_int_const ty v = match vhdl_type_of ty with
         | Unsigned s -> Printf.sprintf "to_unsigned(%d,%d)" v s 
         | Signed s -> Printf.sprintf "to_signed(%d,%d)" v s 
         | Integer _ -> string_of_int v
         | _ -> failwith "Vhdl.scan_match.string_of_int" in
       let scan_pat i (msk,vals,bound_vars) p = match p.p_desc with
           | Pat_int v ->
              msk@[1],
              vals@[string_of_int_const p.p_typ v],
              bound_vars
           | Pat_bool v ->
              msk@[1],
              vals@[string_of_bool v],
              bound_vars
           | Pat_var v ->
              msk@[0],
              vals@[default_value @@ vhdl_type_of p.p_typ],
              bound_vars@[i+1,v]
           | _ ->
              raise (Illegal_pattern pat) in
       let match_msk, match_vals, bound_vars = Misc.list_fold_lefti scan_pat ([],[],[]) ps in 
       Printf.sprintf "%s_match_%s(heap,%s,%s,%s)"
         (string_of_type t)
         c
         (string_of_expr expr)
         (match_msk |> List.map string_of_int |> String.concat "" |> Misc.quote "\"")
         (Misc.string_of_list Fun.id "," match_vals),
       List.map (fun (i,v) -> v,i,expr,pat,t) bound_vars (* resultant bindings *)
    | _ ->
       failwith ("Vhdl.scan_match: " ^ string_of_pattern pat)
                                       
let scan_guard g = match g.Syntax.g_desc with
| Cond e -> string_of_expr e, []
| Match (e,p) -> scan_match e p 

let rec scan_guards gs = match gs with
  | [] -> [],[]
  | g::rest ->
      let cond, bindings = scan_guard g in    
      let conds, bindings' = scan_guards rest in
      cond::conds, bindings @ bindings'

let dump_action oc tab m a =
  fprintf oc "%s%s;\n" tab (string_of_action m a);
  match m.v_has_heap, cfg.trace_heap, a with
  | true, true, Action.Assign (id, expr) ->
     begin match expr.e_desc, vhdl_type_of (expr.e_typ) with
     | ECon1 (c,e'), Variant vd ->
        fprintf oc "%sassert false report \"** heap allocation trigered by: %s\" severity note;\n" tab (Action.to_string a)
     | _ -> ()
     end
  | _ -> ()

let dump_binding oc tab (var,i,exp,pat,ty) =
  let open Syntax in 
  match pat.p_desc with
  | Pat_var _ ->
     Printf.fprintf oc "%s%s := %s;\n" tab var (string_of_expr exp) 
  | Pat_constr1 (c, _) ->
     Printf.fprintf oc "%s%s := %s_get_%s_%d(heap, %s);\n" tab var (string_of_type ty) c i (string_of_expr exp)
  | _ -> failwith "Vhdl.dump_binding" (* should not happen *)

let dump_transition oc tab src m (is_first,_) (_,guards,acts,dst) =
  let conds, binding_acts = scan_guards guards in
  fprintf oc "%s%s ( %s ) then\n" tab (if is_first then "if" else "elsif ") (String.concat " and " conds);
  List.iter (dump_binding oc (tab ^ "  ")) binding_acts;
  List.iter (dump_action oc (tab ^ "  ") m) acts;
  if dst <> src then fprintf oc "%s  %s <= %s;\n" tab cfg.state_var dst;
  (* if m.v_has_heap && cfg.trace_heap then fprintf oc "%s  dump_heap(heap,h_ptr);\n" tab; *)
  (false,true)

let dump_sync_transitions oc src _ m ts =
   let tab = "        " in
   let (_,needs_endif) = List.fold_left (dump_transition oc tab src m) (true,false) ts in
   if needs_endif then fprintf oc "        end if;\n"
     
let dump_state oc m (src,tss) =
  match tss with
  | [] -> failwith ("VHDL: state " ^ src ^ " has no output transition")
  | _ -> dump_sync_transitions oc src false m tss

let dump_state_case oc m (src, tss) =
  fprintf oc "      when %s =>\n" src;
  dump_state oc m (src,tss)

let dump_module_arch oc m =
  let modname = m.v_name in
  fprintf oc "architecture RTL of %s is\n" modname;
  fprintf oc "  type t_%s is ( %s );\n" cfg.state_var (Misc.string_of_list Fun.id ", " m.v_states);
  fprintf oc "  signal %s: t_state;\n" cfg.state_var;
  if cfg.act_sem = Synchronous then 
    List.iter
      (fun (id,(ty,_)) -> fprintf oc "  signal %s: %s;\n" id (string_of_type ~type_marks:TM_Full ty))
      m.v_vars;  (* Note: we do _not_ initialize signals here since this is not always supported by the synthetiser *)
  fprintf oc "begin\n";
  fprintf oc "  process(%s, %s)\n" cfg.reset_sig cfg.clock_sig;
  if cfg.act_sem = Sequential then 
    List.iter
      (fun (id,(ty,_)) -> fprintf oc "    variable %s: %s;\n" id (string_of_type ty))
      m.v_vars;
  List.iter
      (fun (id,ty) -> fprintf oc "    variable %s: %s;\n" id (string_of_type ty))
      m.v_bvars;
  fprintf oc "  begin\n";
  fprintf oc "    if %s='1' then\n" cfg.reset_sig;
  fprintf oc "      %s <= %s;\n" cfg.state_var (fst m.v_init);
  fprintf oc "      rdy <= '1';\n";
  if m.v_has_heap then fprintf oc "      h_ptr <= 0;\n";
  List.iter (dump_action oc "      " m) (snd m.v_init);
  fprintf oc "    elsif rising_edge(%s) then \n" cfg.clock_sig;
  begin match m.v_trans with
    [] -> () (* should not happen *)
  | [c] -> dump_state oc m c 
  | _ -> 
      fprintf oc "      case %s is\n" cfg.state_var;
      List.iter (dump_state_case oc m) m.v_trans;
      fprintf oc "    end case;\n"
  end;
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  if m.v_has_heap then begin
    fprintf oc "  h_hptr <= h_ptr;\n";
    fprintf oc "  h_heap <= heap;\n";
    end;
  fprintf oc "end architecture;\n"

let dump_module_types_package oc m = 
  let pack_name = m.v_name ^ "_types" in
  fprintf oc "package %s is\n" pack_name;
  fprintf oc "  constant %s_heap_size: natural := %d;\n" m.v_name cfg.heap_size;
  fprintf oc "  subtype %s_heap_t is heap_t (0 to %s_heap_size-1);\n" m.v_name m.v_name;
  fprintf oc "  subtype %s_hptr_t is integer range 0 to %s_heap_size-1;\n" m.v_name m.v_name;
  fprintf oc "end package;\n\n";
  cfg.support_packages <- cfg.support_packages @ ["work." ^ pack_name]

let dump_module_intf kind oc m = 
  let modname = m.v_name in
  fprintf oc "%s %s %s\n" kind modname (if kind = "entity" then "is" else "");
  if m.v_has_heap then 
    fprintf oc "  generic (heap_size: natural := %d);\n" cfg.heap_size;
  fprintf oc "  port(\n";
  List.iter (fun (id,ty) -> fprintf oc "        %s: in %s;\n" id (string_of_type ty)) m.v_inps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: out %s;\n" id (string_of_type ty)) m.v_outps;
  fprintf oc "        %s: in std_logic;\n" cfg.clock_sig;
  fprintf oc "        %s: in std_logic\n);\n" cfg.reset_sig;
  fprintf oc "end %s;\n" kind

let dump_libraries ?(extra_pkgs=[]) oc =
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.support_library;
  List.iter
    (fun p -> fprintf oc "use %s.all;\n" p)
    (cfg.support_packages @ List.map (fun p -> "work." ^ p) extra_pkgs);
  fprintf oc "\n"

let dump_model ~pkgs fname m =
  let oc = open_out fname in
  dump_libraries oc;
  List.iter (fprintf oc "use work.%s.all;\n") pkgs;
  fprintf oc "\n";
  if m.v_has_heap then begin
    dump_module_types_package oc m;
    dump_libraries oc;
    List.iter (fprintf oc "use work.%s.all;\n") pkgs;
    end;
  dump_module_intf "entity" oc m;
  fprintf oc "\n";
  dump_module_arch oc m;
  printf "Wrote file %s\n" fname;
  close_out oc

let dump_type_decl oc ty = match ty with 
  | Array(sz,t') ->
     fprintf oc "  type %s is array(0 to %d) of %s;\n" 
       (string_of_vhdl_array_type (sz,t'))
       (sz-1)
       (string_of_vhdl_type t')
  | Variant vd ->
     fprintf oc "  -- type %s is variant(%s);\n"   (* For debug only *)
       (string_of_vhdl_type ty)
       (Misc.string_of_list string_of_variant_ctor_desc " | " vd.vd_ctors)
  | _ ->
     ()

let dump_const_decl oc ~with_val (id,ty,v) = 
  if with_val then Printf.fprintf oc "  constant %s: %s := %s;\n" id (string_of_vhdl_type ty) (string_of_expr v)
  else Printf.fprintf oc "  constant %s: %s;\n" id (string_of_vhdl_type ty)

let write_fsm ?(dir="") ~pkgs ~prefix f = 
  let m = build_model f in
  let p = dir ^ Filename.dir_sep ^ prefix in
  dump_model ~pkgs (p ^ ".vhd") m;
  m.v_name, m

let dump_variant_package oc pkgs t =
  match t with 
  | Variant vd -> 
      let name = vd.vd_name in
      let ctors = vd.vd_ctors in
      let arg_list vc  =
        Misc.string_of_list
          (fun va -> "arg" ^ string_of_int va.va_idx ^ ": " ^ string_of_vhdl_type va.va_typ)
          "; "
          vc.vc_args in  
      let injectors = 
        List.map
          (fun vc ->
            if vc.vc_arity > 0 then
              vc,
              sprintf "procedure %s_mk_%s(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; h_size: in integer; %s; signal result: out value)"
                name
                vc.vc_name
                (arg_list vc)
            else
              vc,
              sprintf "procedure %s_mk_%s (signal heap: inout heap_t; signal h_ptr: inout heap_ptr; unused: boolean; signal result: out value)"
                name
                vc.vc_name)
          ctors in
      let extractors = 
        List.concat_map
          (fun vc ->
            List.map 
              (fun va ->
                vc,
                va,
                sprintf "function %s_get_%s_%d(signal heap: heap_t; v: value) return %s"
                  name vc.vc_name va.va_idx (string_of_vhdl_type va.va_typ))
              vc.vc_args)
        ctors in
      let inspectors = 
        List.map
          (fun vc ->
            if vc.vc_arity > 0 then 
              vc,
              sprintf "function %s_match_%s(signal heap: heap_t; v: value; args: std_logic_vector(1 to %d); %s) return boolean"
                name
                vc.vc_name
                vc.vc_arity
                (arg_list vc)
            else
              vc,
              sprintf "function %s_match_%s(signal heap: heap_t; v: value) return boolean"
                name
                vc.vc_name )
          ctors in
      let printer = sprintf "function %s_to_string(signal heap: heap_t; v: value) return string" name in
      dump_libraries ~extra_pkgs:pkgs oc;
        (* TOFIX: this makes each user-defined type depend on _every_ type previously type.
           A clever approach would associate to each type a list of types on which it depends ! *)
      fprintf oc "package %s is\n" name;
      fprintf oc "  subtype %s is value;\n" name;
      List.iter (fun (vc,s) -> fprintf oc "  %s;\n" s) injectors;
      List.iter (fun (vc,va,s) -> fprintf oc "  %s;\n" s) extractors;
      List.iter (fun (vc,s) -> fprintf oc "  %s;\n" s) inspectors;
      if cfg.target = Flat then fprintf oc "  %s;\n" printer;
      fprintf oc "end package;\n\n";
      dump_libraries ~extra_pkgs:pkgs oc;
        (* TOFIX: this makes each user-defined type depend on _every_ type previously type.
           A clever approach would associate to each type a list of types on which it depends ! *)
      fprintf oc "\n";
      fprintf oc "package body %s is\n" name;
      List.iter
        (fun (vc,intf) ->
          fprintf oc "  %s is\n" intf;
          fprintf oc "  begin\n";
          if vc.vc_arity > 0 then begin
            fprintf oc "    assert h_ptr+%d<h_size report \"%s_mk_%s: cannot allocate, heap is full !\" severity failure;\n"
              vc.vc_arity name vc.vc_name;
            fprintf oc "    heap(h_ptr) <= mk_header(%d, %d);\n" vc.vc_tag vc.vc_arity;
            List.iteri 
              (fun i va ->
                let arg = "arg" ^ string_of_int (i+1) in
                fprintf oc "    heap(h_ptr+%d) <= %s;\n" (i+1) (value_injector va.va_typ arg))
              vc.vc_args;
            fprintf oc "    h_ptr <= h_ptr+%d;\n" (vc.vc_arity+1);
            fprintf oc "    result <= val_ptr(h_ptr);\n";
            if cfg.trace_heap then fprintf oc "  dump_heap(heap,h_ptr);\n"
            end
          else
            fprintf oc "    result <= val_int(%d);\n" vc.vc_tag;
          fprintf oc "  end procedure;\n")
        injectors;
      List.iter
        (fun (vc,va,intf) ->
          fprintf oc "  %s is\n" intf;
          fprintf oc "  begin\n";
          fprintf oc "    return %s;\n" (value_extractor va.va_typ (Printf.sprintf "field(heap,v,%d)" (va.va_idx-1)));
          fprintf oc "  end function;\n")
        extractors;
      List.iter
        (fun (vc,intf) ->
          fprintf oc "  %s is\n" intf;
          if vc.vc_arity > 0 then begin
            fprintf oc "    variable b: block_t;\n";
            fprintf oc "    variable r: boolean;\n";
            fprintf oc "  begin\n";
            fprintf oc "    r := tag_val(heap,v) = %d;\n" vc.vc_tag;
            List.iter 
              (fun va ->
                fprintf oc "    r := r and (args(%d) = '0' or %s_get_%s_%d(heap,v) = arg%d);\n" va.va_idx name vc.vc_name va.va_idx va.va_idx)
              vc.vc_args;
            fprintf oc "    return r;\n";
            fprintf oc "  end function;\n"
            end
          else begin
            fprintf oc "  begin\n";
            fprintf oc "    return is_imm(v) and int_val(v) = %d;\n" vc.vc_tag;
            fprintf oc "  end function;\n"
            end)
        inspectors;
      if cfg.target = Flat then begin
        fprintf oc "  %s is\n" printer;
        fprintf oc "  begin\n";
        List.iter
          (fun vc ->
            if vc.vc_arity > 0 then begin
              fprintf oc "    elsif %s_match_%s(heap, v, \"%s\", %s) then\n"
                name
                vc.vc_name
                (String.make vc.vc_arity '0')
                (Misc.string_of_list (fun va -> Vhdl_types.default_value va.va_typ) "," vc.vc_args);
              fprintf oc "      return \"%s(\" & %s & \")\";\n"
                vc.vc_name
                (Misc.string_of_list
                   (fun va -> to_string_fn va.va_typ (sprintf "%s_get_%s_%d(heap,v)" name vc.vc_name va.va_idx))
                   " & \", \" & "
                   vc.vc_args)
              end
            else
              fprintf oc "    if %s_match_%s(heap,v) then return \"%s\";\n" name vc.vc_name vc.vc_name)
          ctors;
        fprintf oc "    end if;\n";
        fprintf oc "  end function;\n";
        end;
      fprintf oc "end package body;\n";
      fprintf oc "\n";
      name::pkgs
  | _ -> 
     pkgs
    
let write_globals ?(dir=".") ~fname tp p = 
  let used_packages = ref ([]: string list) in
  let typed_consts, arr_types_c = 
    List.fold_left
      (fun (tcs,tys) (id,d) ->
        let open Syntax in
        let ty = vhdl_type_of (List.assoc id tp.Typing.tp_consts) in 
        let tys' = match ty with
        | Array(_,_) when not (List.mem ty tys) -> ty::tys
        | _ -> tys in
        (id,ty,d.cst_desc.c_val) :: tcs,
        tys')
    ([],[])
    p.Syntax.p_consts in
  let arr_types =
    let extract_array_types acc (id,ty) = match vhdl_type_of ty with
      | Array _ as t -> t::acc
      | _ -> acc in
    List.fold_left 
      (fun acc (_,f) -> List.fold_left extract_array_types acc f.Typing.tf_vars) 
      arr_types_c
      tp.tp_fsms in 
  let ud_types = List.fold_left (extract_ud_types tp) [] tp.tp_types in
  let p = dir ^ Filename.dir_sep ^ fname in
  let oc = open_out p in
  if arr_types <> [] then begin
      dump_libraries oc;
      fprintf oc "package %s is\n" cfg.types_pkg_name;
      List.iter (dump_type_decl oc) arr_types;
      fprintf oc "end package;\n\n";
      used_packages := cfg.types_pkg_name :: !used_packages;
      end ;
  if typed_consts <> [] then begin
      dump_libraries oc;
      if arr_types <> [] then fprintf oc "use work.%s.all;\n" cfg.types_pkg_name;
      fprintf oc "package %s is\n" cfg.consts_pkg_name;
      List.iter (dump_const_decl oc ~with_val:false) typed_consts;
      fprintf oc "end package;\n\n";
      dump_libraries oc;
      fprintf oc "package body %s is\n" cfg.consts_pkg_name;
      List.iter (dump_const_decl oc ~with_val:true) typed_consts;
      fprintf oc "end package body;\n";
      used_packages := cfg.consts_pkg_name :: !used_packages;
      end ;
  let other_packages = List.fold_left (dump_variant_package oc) [] ud_types in
  used_packages := !used_packages @ other_packages;
  printf "Wrote file %s\n" p;
  close_out oc;
  !used_packages,
  ud_types

let sig_name m name = Printf.sprintf "%s_%s" m.v_name name

let dump_fsm_signals oc m =
    List.iter
      (fun (name,ty) -> fprintf oc "signal %s: %s;\n" (sig_name m name) (string_of_type ty))
      (m.v_inps @ m.v_outps)

let dump_fsm_inst oc lbl m = 
  fprintf oc "%s: %s port map(%s,%s,%s,%s);\n"
    lbl
    m.v_name
    (Misc.string_of_list (fun (name,_) -> sig_name m name) "," m.v_inps)
    (Misc.string_of_list (fun (name,_) -> sig_name m name) "," m.v_outps)
    cfg.clock_sig
    cfg.reset_sig

let dump_init_signals oc m = 
  Printf.fprintf oc "  %s <= '0';\n" (sig_name m "start");
  if m.v_has_heap then Printf.fprintf oc "  %s <= '0';\n" (sig_name m "h_init")

let dump_inst_outp oc m (o,ty) = 
  let name = sig_name m o in
  match Vhdl_types.vhdl_type_of ty with
  | Variant _ as ty' ->
     fprintf oc "  assert false report \"%s=\" & %s_to_string(%s_h_heap,%s) severity note;\n"
       name
       (Vhdl_types.string_of_vhdl_type ty')
       m.v_name
       name
  | ty' ->
     fprintf oc "  assert false report \"%s=\" & %s_to_string(%s) severity note;\n"
       name
       (Vhdl_types.string_of_vhdl_type ~type_marks:TM_None ty')
       name

let dump_inst_sim oc m vs = 
  fprintf oc "  -- -- start computation\n";
  let open Vhdl_heap in
  let inject_value t v = match v, vhdl_type_of t with
    | Imm (Int v'), Unsigned sz -> sprintf "to_unsigned(%d,%d)" v' sz 
    | Imm (Int v'), Signed sz -> sprintf "to_signed(%d,%d)" v' sz 
    | Imm (Int v'), Integer _ -> sprintf "%d" v'
    | Imm (Bool v'), Std_logic -> sprintf "'%d'" (if v' then 1 else 0)
    | Imm (Float v'), Real -> sprintf "%f" v'
    | Imm (Int c), Variant _ -> sprintf "val_int(%d)" c (* value ctors *)
    | Ptr p, _ -> sprintf "val_ptr(%d)" p
    | _, _ -> failwith "Vhdl.dump_inst_sim.inject_value" in
  try
  List.iter2
    (fun (name,ty) v -> fprintf oc "  %s <= %s;\n" (sig_name m name) (inject_value ty v))
    m.v_inps
    vs;
  with Invalid_argument _ -> ();
  fprintf oc "  %s <= '1';\n" (sig_name m "start");
  fprintf oc "  wait for %d %s;\n" cfg.start_duration cfg.time_unit;
  fprintf oc "  %s <= '0';\n" (sig_name m "start");
  fprintf oc "  wait until %s = '1';\n" (sig_name m "rdy");
  List.iter (dump_inst_outp oc m) @@ List.filter (fun (n,_) -> not (List.mem n ["rdy";"h_heap";"h_hptr"])) m.v_outps;
  if cfg.print_heap_size then
     fprintf oc "  assert false report \"%s_final_heap_size=\" & integer'image(%s_h_hptr) severity note;\n"
       m.v_name 
       m.v_name ;
  if cfg.trace_heap then
    fprintf oc "  dump_heap(%s_h_heap, %s_h_hptr);\n" m.v_name m.v_name;
  fprintf oc "  wait for %d %s;\n" cfg.sim_interval cfg.time_unit

let dump_reset_process oc = 
  fprintf oc "RESET: process\n";
  fprintf oc "begin\n";
  fprintf oc "  %s <= '1';\n" cfg.reset_sig;
  fprintf oc "  wait for %d %s;\n" cfg.reset_duration cfg.time_unit;
  fprintf oc "  %s <= '0';\n" cfg.reset_sig;
  fprintf oc "  wait;\n";
  fprintf oc "end process;\n"

let dump_clock_process oc = 
  fprintf oc "\n";
  fprintf oc "CLOCK: process\n";
  fprintf oc "begin\n";
  fprintf oc "  %s <= '1';\n" cfg.clock_sig;
  fprintf oc "  wait for %d %s;\n" (cfg.clock_period/2) cfg.time_unit;
  fprintf oc "  %s <= '0';\n" cfg.clock_sig;
  fprintf oc "  wait for %d %s;\n" (cfg.clock_period/2) cfg.time_unit;
  fprintf oc "end process;\n"

let dump_heap_init_data oc i heap =
  fprintf oc "  constant heap_init_%d: heap_init_t := (\n" i;
  let size = Array.length heap in
  for i=0 to size-1 do
    fprintf oc "    %s%s -- %d\n" (Vhdl_heap.string_of_word heap.(i)) (if i<size-1 then "," else "") i 
  done;
  fprintf oc "  );\n"

let dump_heap_init_seq oc i m =
  fprintf oc "  -- -- heap init sequence\n";
  fprintf oc "  wait for %d %s;\n" (cfg.clock_period/2) cfg.time_unit;
  fprintf oc "  %s <= '1';\n" (sig_name m "h_init");
  fprintf oc "  %s <= heap_init_%d'length;\n" (sig_name m "h_icnt") i;
  fprintf oc "  wait for %d %s;\n" cfg.clock_period cfg.time_unit;
  fprintf oc "  %s <= '0';\n" (sig_name m "h_init");
  fprintf oc "  for i in heap_init_%d'range loop\n" i;
  fprintf oc "    %s <= heap_init_%d(i);\n" (sig_name m "h_ival") i;
  fprintf oc "    wait for %d %s;\n" cfg.clock_period cfg.time_unit;
  fprintf oc "  end loop;\n";
  fprintf oc "  wait until %s = '1';\n" (sig_name m "rdy")

let dump_sim_process oc variants fsms (fsm,insts) = 
  fprintf oc "process -- %s\n" fsm;
  let m = List.assoc fsm fsms in
  let appls =
    List.mapi
      (fun i ({Syntax.ap_desc=f,args} as appl) ->
        let heap = Vhdl_heap.make cfg.heap_size in
        let arg_vals, heap_size = 
          try Misc.list_map_fold (Vhdl_heap.alloc variants heap) 0 args
          with Vhdl_heap.Heap_full -> raise (Heap_full (Syntax.string_of_appl appl)) in
        i+1, Syntax.string_of_appl appl, arg_vals, Array.sub heap 0 heap_size)
      insts in
  if List.exists (fun (i,lbl,arg_vals,init_heap) -> Array.length init_heap > 0) appls then
    fprintf oc "  type heap_init_t is array (natural range <>) of std_logic_vector(31 downto 0);\n";
  List.iter
    (fun (i,lbl,arg_vals,init_heap) -> 
      if Array.length init_heap > 0 then dump_heap_init_data oc i init_heap)
    appls;
  fprintf oc "begin\n";
  dump_init_signals oc m;
  List.iter
    (fun (i,lbl,arg_vals,init_heap) -> 
      fprintf oc "  -- %s\n" lbl;
      if Array.length init_heap > 0 then begin
        dump_heap_init_seq oc i m;
        fprintf oc "  wait for %d %s;\n" (cfg.clock_period*3/2) cfg.time_unit;
        fprintf oc "  %s <= '0';\n" (sig_name m "h_init");
        if cfg.print_heap_size then
          fprintf oc "  assert false report \"%s_init_heap_size=\" & integer'image(%s_h_hptr) severity note;\n"
            m.v_name 
            m.v_name;
        if m.v_has_heap && cfg.trace_heap then
          fprintf oc "  dump_heap(%s_h_heap, %s_h_hptr);\n" m.v_name m.v_name;
        end
      else
        fprintf oc "  wait for %d %s;\n" cfg.clock_period cfg.time_unit;
      dump_inst_sim oc m arg_vals)
    appls;
  fprintf oc "  assert false report \"end of simulation\" severity note;\n";
  fprintf oc "  wait;\n";
  fprintf oc "end process;\n\n"

let sort_fsm_insts insts = 
  let r = (ref [] : (string * (Syntax.appl list ref)) list ref) in
  List.iter
    (fun ({ Syntax.ap_desc=f,args } as inst) ->
      match List.assoc_opt f !r with
      | Some l -> l := inst::!l
      | None -> r := (f,ref [inst]) :: !r)
    insts;
  List.map (fun (f,appls) -> f, List.rev !appls) !r
             
let write_testbench ~dir ~fname ~pkgs ~variants named_fsms insts = 
  let oc = open_out fname in
  let fsms = List.map snd named_fsms in
  dump_libraries oc;
  List.iter (fprintf oc "use work.%s.all;\n") pkgs;
  fprintf oc "\n";
  fprintf oc "entity %s is\n" cfg.tb_name;
  fprintf oc "end entity;\n";
  fprintf oc "\n";
  fprintf oc "architecture struct of %s is\n" cfg.tb_name;
  fprintf oc "\n";
  List.iter (dump_module_intf "component" oc) fsms;
  fprintf oc "\n";
  List.iter (dump_fsm_signals oc) fsms;
  fprintf oc "signal %s: std_logic;\n" cfg.clock_sig;
  fprintf oc "signal %s: std_logic;\n" cfg.reset_sig;
  fprintf oc "\nbegin\n\n";
  dump_reset_process oc;
  fprintf oc "\n";
  dump_clock_process oc;
  fprintf oc "\n";
  List.iteri (fun i f -> dump_fsm_inst oc ("U" ^ string_of_int (i+1)) f) fsms;
  fprintf oc "\n";
  let sorted_insts = sort_fsm_insts insts in
  List.iter (dump_sim_process oc variants named_fsms) sorted_insts;
  fprintf oc "end architecture;\n";
  printf "Wrote file %s\n" fname;
  close_out oc

