 (* Abstract syntax *)

type state_name = string           
type fsm_name = string           

type type_expr =
  { te_desc: type_expr_desc;
    te_loc: Location.location;
    mutable te_typ: Types.typ }

and type_expr_desc =
  | Typeconstr of string * type_expr list 
  | Typevar of string

type expr = {
  e_desc: e_desc;
  e_loc: Location.location;
  mutable e_typ: Types.typ;
  }

and e_desc = 
| EVar of string
| EInt of int
| EBool of bool
| ETuple of expr list
| EBinop of string * expr * expr

let mk_expr e = { e_desc = e; e_loc = Location.no_location; e_typ = Types.no_type }

(* type 'a located = {
 *   desc: 'a;
 *   loc: Location.location;
 *   } *)

type appl = {
  ap_desc: string * expr list;  (* fsm(args) or state(args) *)
  ap_loc: Location.location;
  }

type fsm_decl = {
  fd_desc: fsm_desc;
  fd_loc: Location.location;
  }
    
and fsm_desc = {
    f_name: string;
    f_params: string list;
    f_desc: state_defn list * appl;  (* LET [state_defns] IN state(args) *)
    mutable f_typ: Types.typ_scheme;
  }

and state_defn = {
  sd_desc: state_name * string list * transition list; (* s(args) = | t1 ... | tn *)
  sd_loc: Location.location;
  mutable sd_typ: Types.typ;
  mutable sd_params: (string * Types.typ) list;
  }

and transition = {
  t_desc: expr * continuation;
  t_loc: Location.location;
  }

and continuation = {
  ct_desc: cont_desc;
  ct_loc: Location.location;
  ct_typ: Types.typ;
  }

and cont_desc =
| Next of appl
| Return of expr

type program = {
    p_fsms: (fsm_name * fsm_decl) list;
    p_insts: appl list;
  }

(* Printing *)

let rec string_of_expr e = string_of_edesc e.e_desc

and string_of_edesc e = match e with
  | EVar v -> v
  | EInt c -> string_of_int c
  | EBool c -> string_of_bool c
  | ETuple es -> "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | EBinop (op, e1, e2) -> string_of_expr e1 ^ op ^ string_of_expr e2 (*TODO : add parens *)

