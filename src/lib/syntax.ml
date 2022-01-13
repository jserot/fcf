 (* Abstract syntax *)

type state_name = string           
type fsm_name = string           
type const_name = string           

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

let mk_expr ty e = { e_desc = e; e_loc = Location.no_location; e_typ = ty }
let mk_bool_expr e = { e_desc = e; e_loc = Location.no_location; e_typ = TyBool }

type const_decl = {
  cst_desc: const_desc;
  cst_loc: Location.location;
  }
    
and const_desc = {
    c_name: string;
    c_val: expr;  (* Will be syntactically limited to scalars and arrays of scalars *)
    c_typ: type_expr;
  }

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
  t_desc: expr * continuation;
  t_loc: Location.location;
  }

and continuation = {
  ct_desc: cont_desc;
  ct_loc: Location.location;
  ct_typ: Types.t;
  }

and cont_desc =
| Next of appl
| Return of expr

type program = {
    p_consts: (const_name * const_decl) list;
    p_fsms: (fsm_name * fsm_decl) list;
    p_insts: appl list;
  }

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

