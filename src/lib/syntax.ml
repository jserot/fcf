 (* Abstract syntax *)

type state_name = string           
type fsm_name = string           

type fsm = {
    f_name: string;
    f_params: string list;
    f_desc: desc
  }

and desc = Let of state_defn list * state_expr

and state_expr = state_name * Expr.t list

and state_defn = state_name * state_descr 

and state_descr = {
    s_params: string list;
    s_trans: transition list
  }

and transition = Expr.t * continuation

and continuation =
| Next of state_expr
| Return of Expr.t

type fsm_expr = fsm_name * Expr.t list

type program = {
    p_fsms: (fsm_name * fsm) list;
    p_exprs: fsm_expr list;
  }

