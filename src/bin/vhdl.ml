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
  mutable use_support_lib: bool;
  mutable support_library: string;
  mutable support_package: string;
  mutable globals_pkg_name: string;
  mutable with_testbench: bool;
  mutable tb_name: string;
  }

and act_semantics =  (** Interpretation of actions associated to transitions *)
  | Sequential        (** sequential (ex: [x:=x+1,y:=x] with [x=1] gives [x=2,y=2]) *)
  | Synchronous       (** synchronous (ex: [x:=x+1,y=x] with [x=1] gives [x=2,y=1]) *)

let cfg = {
  state_var = "state";
  reset_sig = "rst";
  reset_duration = 2;
  clock_sig = "clk";
  clock_period = 10;
  start_duration = 15;
  sim_interval = 50;
  time_unit = "ns";
  act_sem = Synchronous;  (* Default *)
  dump_cc_intf = false;
  use_support_lib = true;
  support_library = "fcf";
  support_package = "fcf";
  globals_pkg_name = "globals";
  with_testbench = false;
  tb_name = "tb";
  }

type model = {
  v_name: string;
  v_states: string list;
  v_inps: (string * Types.t) list;
  v_outps: (string * Types.t) list;
  v_vars: (string * Types.t) list;  
  v_init: State.t * Action.t list;
  v_trans: (State.t * Transition.t list) list; (* Transitions, here indexed by source state *)
  }

module StateSet = Set.Make (struct type t = string let compare = compare end)

let state_id = String.capitalize_ascii

let build_model f = 
  let open Fsm in
  let src_states = 
    List.fold_left 
      (fun acc (src,_,_,_) -> StateSet.add src acc)
      StateSet.empty
      f.m_trans
    |> StateSet.elements in
  { v_name = f.m_name;
    v_states = List.map state_id f.m_states;
    v_inps = f.m_inps;
    v_outps = f.m_outps;
    v_vars = f.m_vars;
    v_init = state_id @@ fst f.m_itrans, snd f.m_itrans;
    v_trans =
      List.map
        (fun s ->
          let ts = List.filter (fun (s',_,_,_) -> s=s') f.m_trans in
          state_id s, List.map (fun (s,guard,acts,s') -> state_id s, guard, acts, state_id s') ts)
        src_states
    }

exception Error of string * string  (* where, msg *)

type vhdl_type = 
  | Unsigned of int
  | Signed of int
  | Integer of int_range option
  | Std_logic
  | Real
  | NoType  (* for debug only *)
  | Array of int * vhdl_type

and int_range = int * int (* lo, hi *)             

let rec vhdl_type_of t =
  let open Types in
  match real_type t with 
  | TyBool -> Std_logic
  | TyFloat -> Real
  | TyInt (Const Unsigned, Const sz) -> Unsigned sz
  | TyInt (Const Signed, Const sz) -> Signed sz
  | TyInt (_, Const sz) -> Signed sz  (* [int<n>] is interpreted as [signed<n>] *)
  | TyInt (_, _) -> Integer None
  | TyProduct [] -> NoType                   
  | TyArr (Const sz, t') when is_scalar_type t' -> Array (sz, vhdl_type_of t') 
  | t -> failwith ("VHDL backend: illegal type: " ^ Types.string_of_type t)

type type_mark = TM_Full | TM_Abbr | TM_None [@@warning "-37"]

let rec string_of_vhdl_type ?(type_marks=TM_Full) t = match t, type_marks with 
  | Unsigned n, TM_Full -> Printf.sprintf "unsigned(%d downto 0)" (n-1)
  | Unsigned n, TM_Abbr -> Printf.sprintf "unsigned%d" n
  | Unsigned _, TM_None -> "unsigned"
  | Signed n, TM_Full -> Printf.sprintf "signed(%d downto 0)" (n-1)
  | Signed n, TM_Abbr -> Printf.sprintf "signed%d" n
  | Signed _, TM_None -> "signed"
  | Integer (Some (lo,hi)), TM_Full -> Printf.sprintf "integer range %d to %d" lo hi
  | Integer _, _ -> "integer"
  | Std_logic, _ -> "std_logic"
  | Real, _ -> "real"
  | Array (sz,t'), _ -> string_of_vhdl_array_type (sz,t') 
  | NoType, _ -> "<unknown>"

and string_of_type ?(type_marks=TM_Full) t =
  string_of_vhdl_type ~type_marks:type_marks (vhdl_type_of t)

and string_of_vhdl_array_type (sz,t) = 
  Printf.sprintf "%s_arr%d" (string_of_vhdl_array_subtype t) sz

and string_of_vhdl_array_subtype t = match t with 
  | Unsigned n -> "u" ^ string_of_int n
  | Signed n -> "s" ^ string_of_int n
  | Integer _ -> "int"
  | Std_logic -> "sl"
  | Real -> "r"
  | _ -> failwith ("VHDL backend: illegal subtype: " ^ string_of_vhdl_type t)

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
    | Syntax.EBinop (">>",e1,e2), ty -> string_of_shift level "shift_left" e1 e2 (* Special cases *)
    | Syntax.EBinop ("<<",e1,e2), ty -> string_of_shift level "shift_right" e1 e2
    | Syntax.EBinop (op,e1,e2), ty -> 
       let s1 = string_of (level+1) e1 
       and s2 = string_of (level+1) e2 in 
       begin match op, ty with
       | "*", Signed _
       | "*", Unsigned _ -> "mul(" ^ s1 ^ "," ^ s2 ^ ")"
       | _, _ -> paren level (s1 ^ string_of_op op ^ s2)
       end
    | Syntax.EArray es, _ -> Printf.sprintf "(%s)" (Misc.string_of_list string_of_expr "," es)
    | Syntax.EArrRd (a,idx), _ -> Printf.sprintf "%s(%s)" a (string_of level idx)
    | _ -> Misc.fatal_error "Vhdl.string_of_expr"
  and string_of_shift level op e1 e2 =
    op ^ "(" ^ string_of (level+1) e1 ^ "," ^ string_of_int_expr (level+1) e2 ^ ")"
  and string_of_int_expr level e = match vhdl_type_of e.Syntax.e_typ with 
      | Integer _ -> string_of level e
      | Unsigned _ | Signed _ -> "to_integer(" ^ string_of level e ^ ")"
      | _ -> Misc.fatal_error "Vhdl.string_of_expr"  (* should not happen *)
  in
  string_of 0 e

let string_of_action m a =
  let asn id = " <= " in
  let string_of_act id expr = id ^ asn id ^ string_of_expr expr in
  match a with
  | Action.Assign (id, expr) ->
     begin match expr.e_desc with
     | ETuple es when id = Fsm.cfg.Fsm.res_id  -> (* Special case *)
        Misc.string_of_list
          Fun.id
          "; " 
          (List.mapi (fun i e -> string_of_act (id ^ string_of_int (i+1)) e) es)
     | _ ->
        string_of_act id expr
     end

let string_of_guard g = match g.Syntax.g_desc with
| Cond e -> string_of_expr e
| Match _ -> "<match>" (* TO BE FIXED *)

let string_of_guards gs =  
  Misc.string_of_list string_of_guard " and " gs

let dump_action oc tab m a = fprintf oc "%s%s;\n" tab (string_of_action m a)

let dump_transition oc tab src m (is_first,_) (_,guard,acts,dst) =
       fprintf oc "%s%s ( %s ) then\n" tab (if is_first then "if" else "elsif ") (string_of_guards [guard]);
       List.iter (dump_action oc (tab ^ "  ") m) acts;
       if dst <> src then fprintf oc "%s  %s <= %s;\n" tab cfg.state_var dst;
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
      (fun (id,ty) -> fprintf oc "  signal %s: %s;\n" id (string_of_type ~type_marks:TM_Full ty))
      m.v_vars;
  fprintf oc "begin\n";
  fprintf oc "  process(%s, %s)\n" cfg.reset_sig cfg.clock_sig;
  if cfg.act_sem = Sequential then 
    List.iter
      (fun (id,ty) -> fprintf oc "    variable %s: %s;\n" id (string_of_type ty))
      m.v_vars;
  fprintf oc "  begin\n";
  fprintf oc "    if %s='1' then\n" cfg.reset_sig;
  fprintf oc "      %s <= %s;\n" cfg.state_var (fst m.v_init);
  fprintf oc "      rdy <= '1';\n";
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
  fprintf oc "end architecture;\n"

let dump_module_intf kind oc m = 
  let modname = m.v_name in
  fprintf oc "%s %s %s\n" kind modname (if kind = "entity" then "is" else "");
  fprintf oc "  port(\n";
  List.iter (fun (id,ty) -> fprintf oc "        %s: in %s;\n" id (string_of_type ty)) m.v_inps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: out %s;\n" id (string_of_type ty)) m.v_outps;
  fprintf oc "        %s: in std_logic;\n" cfg.clock_sig;
  fprintf oc "        %s: in std_logic\n);\n" cfg.reset_sig;
  fprintf oc "end %s;\n" kind

let dump_libraries oc =
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  fprintf oc "use ieee.numeric_std.all;\n";
  if cfg.use_support_lib then begin
    fprintf oc "library %s;\n" cfg.support_library;
    fprintf oc "use %s.%s.all;\n" cfg.support_library cfg.support_package
    end
  else
    fprintf oc "use work.%s.all;\n" cfg.support_package

let dump_model ~has_globals fname m =
  let oc = open_out fname in
  dump_libraries oc;
  if has_globals then fprintf oc "use work.%s.all;\n" cfg.globals_pkg_name;
  fprintf oc "\n";
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
  | _ ->
     ()

let dump_const_decl oc ~with_val (id,ty,v) = 
  if with_val then Printf.fprintf oc "  constant %s: %s := %s;\n" id (string_of_vhdl_type ty) (string_of_expr v)
  else Printf.fprintf oc "  constant %s: %s;\n" id (string_of_vhdl_type ty)

let write_fsm ?(dir="") ~has_globals ~prefix f = 
  let m = build_model f in
  let p = dir ^ Filename.dir_sep ^ prefix in
  dump_model ~has_globals (p ^ ".vhd") m;
  m.v_name, m

let write_globals ?(dir="") ~fname typed_consts consts = 
  let typed_consts, arr_types = 
    List.fold_left
      (fun (tcs,tys) (id,d) ->
        let open Syntax in
        let ty = vhdl_type_of (List.assoc id typed_consts) in 
        let tys' = match ty with
        | Array(_,_) when not (List.mem ty tys) -> ty::tys
        | _ -> tys in
        (id,ty,d.cst_desc.c_val) :: tcs,
        tys')
    ([],[])
    consts in
  let oc = open_out fname in
  dump_libraries oc;
  fprintf oc "\n";
  fprintf oc "package %s is\n" cfg.globals_pkg_name;
  List.iter (dump_type_decl oc) arr_types;
  List.iter (dump_const_decl oc ~with_val:false) typed_consts;
  fprintf oc "end package;\n\n";
  dump_libraries oc;
  fprintf oc "\n";
  fprintf oc "package body %s is\n" cfg.globals_pkg_name;
  List.iter (dump_const_decl oc ~with_val:true) typed_consts;
  fprintf oc "end package body;\n";
  printf "Wrote file %s\n" fname;
  close_out oc

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

let dump_init_signals oc (_,m) = 
  Printf.fprintf oc "  %s <= '0';\n" (sig_name m "start")

let dump_inst_sim oc fsms { Syntax.ap_desc = (f,vs) } =
  fprintf oc "  --  %s(%s)\n" f (Misc.string_of_list string_of_expr "," vs);
  let m = List.assoc f fsms in
  try
    List.iter2
      (fun (name,ty) v -> fprintf oc "  %s <= %s;\n" (sig_name m name) (string_of_expr v))
      m.v_inps
      vs
  with 
    Invalid_argument _ -> ();
  fprintf oc "  %s_start <= '1';\n" m.v_name;
  fprintf oc "  wait for %d %s;\n" cfg.start_duration cfg.time_unit;
  fprintf oc "  %s_start <= '0';\n" m.v_name;
  fprintf oc "  wait until %s_rdy = '1';\n" m.v_name;
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
  fprintf oc "  wait for %d %s;\n" cfg.clock_period cfg.time_unit;
  fprintf oc "  %s <= '0';\n" cfg.clock_sig;
  fprintf oc "  wait for %d %s;\n" cfg.clock_period cfg.time_unit;
  fprintf oc "end process;\n"

let dump_sim_process oc fsms insts = 
  fprintf oc "process\n";
  fprintf oc "begin\n";
  List.iter (dump_init_signals oc) fsms;
  fprintf oc "  wait for %d %s;\n" cfg.clock_period cfg.time_unit;
  List.iter (dump_inst_sim oc fsms) insts;
  fprintf oc "  assert false report \"end of simulation\" severity note;\n";
  fprintf oc "  wait;\n";
  fprintf oc "end process;\n\n"

let write_testbench ~dir ~fname ~has_globals named_fsms insts = 
  let oc = open_out fname in
  let fsms = List.map snd named_fsms in
  dump_libraries oc;
  if has_globals then fprintf oc "use work.%s.all;\n" cfg.globals_pkg_name;
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
  fprintf oc "\n\n";
  dump_sim_process oc named_fsms insts;
  fprintf oc "end architecture;\n";
  printf "Wrote file %s\n" fname;
  close_out oc
