 (* Abstract syntax *)

type state_name = string           
type fsm_name = string           

type expr = {
  e_desc: e_desc;
  e_loc: Location.location;
  }

and e_desc = 
| EVar of string
| EInt of int
| EPrim of string * expr list

let mk_expr e = { e_desc = e; e_loc = Location.no_location }

type fsm_decl = {
  fd_desc: fsm_desc;
  fd_loc: Location.location;
  }
    
and fsm_desc = {
    f_name: string;
    f_params: string list;
    f_desc: state_decl list * state_expr;  (* LET [state_defns] IN state(args) *)
  }

and state_expr = state_name * expr list

and state_decl = {
  sd_desc: state_name * state_descr;
  sd_loc: Location.location;
  }

and state_descr = {
  sd_params: string list;
  sd_trans: transition list
  }

and transition = {
  t_desc: expr * continuation;
  t_loc: Location.location;
  }

and continuation = {
  ct_desc: cont_desc;
  ct_loc: Location.location;
  }

and cont_desc =
| Next of state_expr
| Return of expr

type fsm_inst = {
  fi_desc: fsm_name * expr list;
  fi_loc: Location.location;
  }

type program = {
    p_fsms: (fsm_name * fsm_decl) list;
    p_insts: fsm_inst list;
  }

(* Printing *)

let rec string_of_expr e = string_of_edesc e.e_desc

and string_of_edesc e = match e with
  | EVar v -> v
  | EInt c -> string_of_int c
  | EPrim (p, es) -> p ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"

