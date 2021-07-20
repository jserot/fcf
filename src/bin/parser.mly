(* %token TYPE *)
%token LET
%token IN
%token AND
%token RETURN
%token <int> INT
(* %token <bool> BOOL *)
%token <string> LID
%token SEMICOLON
%token BAR
%token COMMA
%token ARROW
(* %token COLON *)
%token EQUAL
%token NOTEQUAL
%token LPAREN
%token RPAREN
%token LT
%token GT
%token LTE
%token GTE
%token PLUS MINUS TIMES DIV
%token EOF

(* Precedences and associativities for expressions *)

%left EQUAL NOTEQUAL GT LT GTE LTE
(* %left SHR SHL
 * %left LAND LOR LXOR *)
%left PLUS MINUS
%left TIMES DIV
(* %nonassoc prec_unary_minus         (\* Highest precedence *\) *)

%start <Syntax.program> program

%{
open Fcf
open Syntax

(* let mk_typed_expr e ty = let open Expr in { e_desc = e; e_typ = ty }
 * let mk_int_expr e = mk_typed_expr e (Types.type_int ())
 * let mk_bool_expr e = mk_typed_expr e Types.TyBool
 * let mk_expr e = mk_typed_expr e (Types.TyVar (Types.new_type_var ())) *)
let mk_expr e =  e
%}

%%

(* %public optional(X):
 *     /* Nothing */ { [] }
 *   | x=X { x } *)

program:
  | fs=nonempty_list(fsm) es=list(fsm_expr) EOF { { p_fsms=fs; p_exprs=es } }
        
fsm:
  | LET name=LID ps=params EQUAL ss=states IN s=state_expr SEMICOLON
      { name, { f_name=name; f_params=ps; f_desc=Let(ss,s) } }

fsm_expr:
  | f=LID es=args SEMICOLON { f, es }

params:
  | (* Nothing *) { [] }
  | LPAREN ps=separated_list(COMMA, LID) RPAREN { ps }

states:
  | LET ss=separated_nonempty_list(AND, state) { ss }

state:
  | name=LID ps=params EQUAL ts=nonempty_list(transition) { name, {s_params=ps; s_trans=ts} } 

transition:
  | BAR e=guard ARROW c=continuation { (e,c) }

guard:
  | e=expr { e }
        
continuation:
  | RETURN e=expr { Return e }
  | e=state_expr { Next e }

state_expr:
  | s=LID es=args { s, es }

args:
  | (* Nothing *) { [] }
  | LPAREN es=separated_list(COMMA, expr) RPAREN { es }


(* EXPRESSIONS *)

expr:
  | e = simple_expr
      { e }
  | e1 = expr PLUS e2 = expr
      { mk_expr (Expr.EPrim ("+", [e1; e2])) }
  | e1 = expr MINUS e2 = expr
      { mk_expr (Expr.EPrim ("-", [e1; e2])) }
  | e1 = expr TIMES e2 = expr
      { mk_expr (Expr.EPrim ("*", [e1; e2])) }
  | e1 = expr DIV e2 = expr
      { mk_expr (Expr.EPrim ("/", [e1; e2])) }
  | e1 = expr EQUAL e2 = expr
      { mk_expr (Expr.EPrim ("=", [e1; e2])) }
  | e1 = expr NOTEQUAL e2 = expr
      { mk_expr (Expr.EPrim ("!=", [e1; e2])) }
  | e1 = expr GT e2 = expr
      { mk_expr (Expr.EPrim (">", [e1; e2])) }
  | e1 = expr LT e2 = expr
      { mk_expr (Expr.EPrim ("<", [e1; e2])) }
  | e1 = expr GTE e2 = expr
      { mk_expr (Expr.EPrim (">=", [e1; e2])) }
  | e1 = expr LTE e2 = expr
      { mk_expr (Expr.EPrim ("<=", [e1; e2])) }

simple_expr:
  | v = LID
      { mk_expr (Expr.EVar v) }
  | c = INT
      { mk_expr (Expr.EInt c) }
  (* | c = BOOL
   *     { mk_expr (Expr.EBool c) } *)
  | MINUS c=INT
      { mk_expr (Expr.EInt (-c)) }
  | LPAREN e = expr RPAREN
      { e }
