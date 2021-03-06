 (* Abstract syntax *)

type state_name = string           
type fsm_name = string           
type const_name = string           
type type_name = string           

let array_print_length = ref 8

type type_expr =
  { te_desc: type_expr_desc;
    te_loc: Location.location;
    mutable te_typ: Types.t }

and type_expr_desc =
  | TeInt of int_sign option * int_size option
  | TeBool 
  | TeFloat
  | TeArray of int * type_expr 
  | TeConstr of string * type_expr list         (* Ex: int list *)
  | TeVar of string 

and int_sign = TeSigned | TeUnsigned 
and int_size = TeWidth of int | TeRange of int * int

type expr = {
  e_desc: e_desc;
  e_loc: Location.location;
  mutable e_typ: Types.t;
  }

and e_desc = 
| EVar of string
| EInt of int
| EBool of bool
| EFloat of float
| ETuple of expr list
| EBinop of string * expr * expr
| EArray of expr array
| EArrRd of string * expr (* arr[idx] *)
| ECon0 of string
| ECon1 of string * expr
| ECast of expr * type_expr (* e:>ty *)

type pattern =
  { p_desc: pattern_desc;
    p_loc: Location.location;
    mutable p_typ: Types.t }

and pattern_desc =
  | Pat_var of string
  | Pat_bool of bool
  | Pat_int of int
  | Pat_tuple of pattern list
  | Pat_constr0 of string
  | Pat_constr1 of string * pattern

type const_decl = {
  cst_desc: const_desc;
  cst_loc: Location.location;
  }
    
and const_desc = {
    c_name: string;
    c_val: expr;  (* Will be syntactically limited to scalars and arrays of scalars *)
    c_typ: type_expr;
  }

type var_decl = {
  v_desc: string * type_expr * expr option;
  v_loc: Location.location;
  mutable v_typ: Types.t
  }
    
type lhs =
  | LVar of string         (* v := ... *)
  | LArr of string * expr  (* a[i] := ... *)

type type_defn = 
| Variant_decl of constr_decl list

and constr_decl =
  | Constr0_decl of string
  | Constr1_decl of string * type_expr list

and type_param = string

type appl = {
  ap_desc: string * expr list;  (* fsm(args) or state(args) *)
  ap_loc: Location.location;
  }

type param = string * type_expr option

type fsm_decl = {
  fd_desc: fsm_desc;
  fd_loc: Location.location;
  }
    
and fsm_desc = {
    f_name: string;
    f_params: param list;
    f_vars: var_decl list; 
    f_desc: state_defn list * appl;  (* LET [state_defns] IN state(args) *)
    mutable f_typ: Types.typ_scheme;
  }

and state_defn = {
  sd_desc: state_name * param list * transition list; (* s(args) = | t1 ... | tn *)
  sd_loc: Location.location;
  mutable sd_typ: Types.t;
  mutable sd_params: (string * Types.t) list;
  }

and transition = {
  t_desc: guard list * action list * continuation;
  t_loc: Location.location;
  }

and guard = {
  g_desc: guard_desc;
  g_loc: Location.location;
  }

and guard_desc = 
| Cond of expr
| Match of expr * pattern 

and action = {
  ac_desc: action_desc;
  ac_loc: Location.location;
  }

and action_desc = lhs * expr 

and continuation = {
  ct_desc: cont_desc;
  ct_loc: Location.location;
  ct_typ: Types.t;
  }

and cont_desc =
| Next of appl
| Return of expr

type type_decl = {
  td_desc: type_param list * type_defn;
  td_loc: Location.location;
  }

type program = {
    p_types: (string * type_decl) list;
    p_consts: (string * const_decl) list;
    p_fsms: (string * fsm_decl) list;
    p_insts: (top_symbol list * appl) list; (* lhs[s] = fsm(args) *)
  }

and top_symbol = {
  top_id: string;
  mutable top_typ: Types.t;

  }
(* Renaming *)

let rec rename_trans_vars f t = { t with t_desc = rename_trans_desc_vars f t.t_desc }

and rename_trans_desc_vars f (guards,acts,cont) =
  List.map (rename_guard_vars f) guards,
  List.map (rename_action_vars f) acts,
  rename_cont_vars f cont

and rename_guard_vars f g = { g with g_desc = rename_guard_desc_vars f g.g_desc }

and rename_guard_desc_vars f g = match g with 
| Cond e -> Cond (rename_expr_vars f e)
| Match (e,p) -> Match (rename_expr_vars f e, rename_pattern_vars f p)

and rename_action_vars f a = { a with ac_desc = rename_action_desc_vars f a.ac_desc }

and rename_action_desc_vars f (lhs,exp) = rename_lhs_vars f lhs, rename_expr_vars f exp

and rename_expr_vars f e = { e with e_desc = rename_expr_desc_vars f e.e_desc }

and rename_expr_desc_vars f e = match e with 
  | EVar v -> EVar (f v)
  | ETuple es -> ETuple (List.map (rename_expr_vars f) es)
  | EBinop (op, e1, e2) -> EBinop (op, rename_expr_vars f e1, rename_expr_vars f e2)
  | EArray vs -> EArray (Array.map (rename_expr_vars f) vs)
  | EArrRd (a,i) -> EArrRd (a, rename_expr_vars f i)
  | ECon1 (c,e) -> ECon1 (c, rename_expr_vars f e)
  | ECast (e,t) -> ECast (rename_expr_vars f e, t)
  | e -> e

and rename_pattern_vars f p = { p with p_desc = rename_pattern_desc_vars f p.p_desc }

and rename_pattern_desc_vars f p = match p with 
  | Pat_var v -> Pat_var (f v)
  | Pat_tuple ps -> Pat_tuple (List.map (rename_pattern_vars f) ps)
  | Pat_constr1 (c,p) -> Pat_constr1 (c, rename_pattern_vars f p)
  | p -> p
 
and rename_cont_vars f c = { c with ct_desc = rename_cont_desc_vars f c.ct_desc }

and rename_cont_desc_vars f c = match c with 
  | Next a -> Next (rename_appl_vars f a)
  | Return e -> Return (rename_expr_vars f e)

and rename_appl_vars f a = { a with ap_desc = rename_appl_desc_vars f a.ap_desc }

and rename_appl_desc_vars f (a,exprs) = a, List.map (rename_expr_vars f) exprs

and rename_lhs_vars f lhs = match lhs with
  | LVar v -> LVar (f v)
  | LArr (a,i) -> LArr (f a, rename_expr_vars f i)

(* Helpers *)

let mk_expr ?(ty=Types.no_type) e = { e_desc = e; e_loc = Location.no_location; e_typ = ty }
let mk_bool_expr e = { e_desc = e; e_loc = Location.no_location; e_typ = TyBool }
let mk_cond_guard e = { g_desc = Cond e; g_loc = Location.no_location }
let mk_binop_guard op e1 e2 = mk_cond_guard @@ mk_bool_expr @@ EBinop(op, mk_expr e1, mk_expr e2)

(* Printing *)

let rec string_of_type_expr te = string_of_tedesc te.te_desc

and string_of_tedesc te = match te with
  | TeInt (sg, sz) -> string_of_sign sg ^ string_of_size sz
  | TeBool -> "bool" 
  | TeFloat -> "float"
  | TeArray (sz, t') -> string_of_type_expr t' ^ " array" ^ string_of_int sz
  | TeConstr (c,[]) -> c
  | TeConstr (c,ts) ->  Misc.string_of_list string_of_type_expr "_" ts ^ "_" ^ c
  | TeVar v -> v

and string_of_sign sg = match sg with
  | Some TeUnsigned -> "unsigned"
  | Some TeSigned -> "signed"
  | None -> "int"

and string_of_size sz =
  let string_of_sz s = match s with
    | TeWidth s -> string_of_int s
    | TeRange (lo,hi) -> string_of_int lo ^ ":" ^ string_of_int hi in
  match sz with
  | Some s -> "<" ^ string_of_sz s ^ ">"
  | None -> ""
let rec string_of_expr e = string_of_edesc e.e_desc

and string_of_edesc e = match e with
  | EVar v -> v
  | EInt c -> string_of_int c
  | EBool c -> string_of_bool c
  | EFloat c -> string_of_float c
  | ETuple es -> "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | EBinop (op, e1, e2) -> string_of_expr e1 ^ op ^ string_of_expr e2 (*TODO : add parens *)
  | EArray vs -> "{" ^ Misc.string_of_array ~max_elems:(!array_print_length) string_of_expr ","  vs ^ "}"
  | EArrRd (a,i) -> a ^ "[" ^ string_of_expr i ^ "]"
  | ECon0 c -> c 
  | ECon1 (c,e) -> c ^ " " ^ string_of_expr e
  | ECast (e,t) -> string_of_expr e ^ ":>" ^ string_of_type_expr t

let rec string_of_pattern p = string_of_pdesc p.p_desc 

and string_of_pdesc p = match p with 
  | Pat_var v -> v
  | Pat_bool b -> string_of_bool  b
  | Pat_int i -> string_of_int i
  | Pat_tuple [] -> "()" 
  | Pat_tuple [p] -> string_of_pattern p
  | Pat_tuple ps -> "(" ^ Misc.string_of_list string_of_pattern "," ps ^ ")"
  | Pat_constr0 c -> c
  | Pat_constr1 (c,p) -> c ^ " " ^ string_of_pattern p

let string_of_guard g = match g.g_desc with
| Cond e -> string_of_expr e
| Match (e,p) -> string_of_expr e ^ "~" ^ string_of_pattern p

let string_of_appl_desc (fsm_id,exprs)  = fsm_id ^ "(" ^ Misc.string_of_list string_of_expr "," exprs ^ ")"
                                        
let string_of_appl a = string_of_appl_desc a.ap_desc

let string_of_lhs lhs = match lhs with
  | LVar v -> v
  | LArr (a,i) -> a ^ "[" ^ string_of_expr i ^ "]"

let string_of_top_lhs ?(with_type=false) lhs = match lhs, with_type with
  | [l], false -> l.top_id
  | [l], true -> Printf.sprintf "%s : %s" l.top_id (Types.string_of_type l.top_typ)
  | ls, false -> Misc.string_of_list (fun l -> l.top_id) "," ls
  | ls, true -> Printf.sprintf "%s : %s"
                  (Misc.string_of_list (fun l -> l.top_id) "," ls)
                  (Misc.string_of_list (fun l -> Types.string_of_type l.top_typ) "*" ls)

let string_of_inst (lhs,appl) = string_of_top_lhs lhs ^ "=" ^ string_of_appl appl
