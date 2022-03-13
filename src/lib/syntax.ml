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
and int_size = int

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
| EArray of expr list
| EArrRd of string * expr (* arr[idx] *)
| ECon0 of string
| ECon1 of string * expr

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
  t_desc: guard list * continuation;
  t_loc: Location.location;
  }

and guard = {
  g_desc: guard_desc;
  g_loc: Location.location;
  }

and guard_desc = 
| Cond of expr
| Match of expr * pattern 

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
    p_insts: appl list;
  }

(* Helpers *)

let mk_expr ?(ty=Types.no_type) e = { e_desc = e; e_loc = Location.no_location; e_typ = ty }
let mk_bool_expr e = { e_desc = e; e_loc = Location.no_location; e_typ = TyBool }
let mk_cond_guard e = { g_desc = Cond e; g_loc = Location.no_location }
let mk_binop_guard op e1 e2 = mk_cond_guard @@ mk_bool_expr @@ EBinop(op, mk_expr e1, mk_expr e2)

(* Printing *)

let rec string_of_expr e = string_of_edesc e.e_desc

and string_of_edesc e = match e with
  | EVar v -> v
  | EInt c -> string_of_int c
  | EBool c -> string_of_bool c
  | EFloat c -> string_of_float c
  | ETuple es -> "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | EBinop (op, e1, e2) -> string_of_expr e1 ^ op ^ string_of_expr e2 (*TODO : add parens *)
  | EArray vs -> "{" ^ Misc.string_of_list ~max_elems:(!array_print_length) string_of_expr ","  vs ^ "}"
  | EArrRd (a,i) -> a ^ "[" ^ string_of_expr i ^ "]"
  | ECon0 c -> c 
  | ECon1 (c,e) -> c ^ " " ^ string_of_expr e

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
