(* %token TYPE *)
%token LET
%token IN
%token AND
%token RETURN
%token <int> INT
%token TRUE
%token FALSE
%token <string> LID
%token SEMICOLON
%token BAR
%token COMMA
%token ARROW
(* %token COLON *)
(* %token QUOTE *)
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
open Location

(* let mk_typed_expr e ty = let open Expr in { e_desc = e; e_typ = ty }
 * let mk_int_expr e = mk_typed_expr e (Types.type_int ())
 * let mk_bool_expr e = mk_typed_expr e Types.TyBool
 * let mk_expr e = mk_typed_expr e (Types.TyVar (Types.new_type_var ())) *)

let mk_location (p1,p2) =
  let open Lexing in
  Loc (!input_name, p1.pos_cnum, p2.pos_cnum)

let mk_type_expr l desc = { te_desc = desc; te_loc = mk_location l; te_typ = Types.no_type }
let mk_appl l desc = { ap_desc = desc; ap_loc = mk_location l }
let mk_fsm_decl l desc = { fd_desc = desc; fd_loc = mk_location l }
let mk_state_decl l desc = { sd_desc = desc; sd_loc = mk_location l; sd_typ = Types.no_type; sd_params = [] }
let mk_transition l desc = { t_desc = desc; t_loc = mk_location l }
let mk_continuation l desc = { ct_desc = desc; ct_loc = mk_location l; ct_typ = Types.no_type }
let mk_expr l desc = { e_desc = desc; e_loc = mk_location l; e_typ = Types.no_type }
%}

%%

(* %public optional(X):
 *     /* Nothing */ { [] }
 *   | x=X { x } *)

program:
  | fs=nonempty_list(fsm_decl) es=list(fsm_inst) EOF { { p_fsms=fs; p_insts=es } }
        
fsm_decl:
  | LET name=LID ps=params EQUAL ss=states IN s=state_expr SEMICOLON
      { name, mk_fsm_decl $sloc { f_name=name; f_params=ps; f_desc=ss,s; f_typ=Types.no_type_scheme }  }

fsm_inst:
  | f=LID es=args SEMICOLON { mk_appl $sloc (f,es) }

params:
  | (* Nothing *) { [] }
  | LPAREN ps=separated_list(COMMA, LID) RPAREN { ps }

(* typed_id:
 *   | id=LID COLON t=type_expr { id, t } *)

states:
  | LET ss=separated_nonempty_list(AND, state) { ss }

state:
  | name=LID ps=params EQUAL ts=nonempty_list(transition)
     { mk_state_decl $sloc (name, ps, ts) } 

transition:
  | BAR e=guard ARROW c=continuation { mk_transition $sloc (e,c) }

guard:
  | e=expr { e }
        
continuation:
  | RETURN e=expr { mk_continuation $sloc (Return e) }
  | e=state_expr { mk_continuation $sloc (Next e) }

state_expr:
  | s=LID es=args { mk_appl $sloc (s,es) }

args:
  | (* Nothing *) { [] }
  | LPAREN es=separated_list(COMMA,expr) RPAREN { es }

(* EXPRESSIONS *)

expr:
  | e = simple_expr
      { e }
  (* | es=expr_comma_list
   *     { mk_expr $sloc (ETuple (List.rev es)) } *)
  | e1 = expr PLUS e2 = expr
      { mk_expr $sloc (EBinop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr
      { mk_expr $sloc (EBinop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr
      { mk_expr $sloc (EBinop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr
      { mk_expr $sloc (EBinop ("/", e1, e2)) }
  | e1 = expr EQUAL e2 = expr
      { mk_expr $sloc (EBinop ("=", e1, e2)) }
  | e1 = expr NOTEQUAL e2 = expr
      { mk_expr $sloc (EBinop ("!=", e1, e2)) }
  | e1 = expr GT e2 = expr
      { mk_expr $sloc (EBinop (">", e1, e2)) }
  | e1 = expr LT e2 = expr
      { mk_expr $sloc (EBinop ("<", e1, e2)) }
  | e1 = expr GTE e2 = expr
      { mk_expr $sloc (EBinop (">=", e1, e2)) }
  | e1 = expr LTE e2 = expr
      { mk_expr $sloc (EBinop ("<=", e1, e2)) }

(* expr_comma_list:
 *         expr_comma_list COMMA expr
 *           { $3 :: $1 }
 *       | expr COMMA expr
 *           { [$3; $1] } *)

simple_expr:
  | v = LID
      { mk_expr $sloc (EVar v) }
  | c = INT
      { mk_expr $sloc (EInt c) }
  | TRUE
      { mk_expr $sloc (EBool true) }
  | FALSE
      { mk_expr $sloc (EBool false) }
  | MINUS c=INT
      { mk_expr $sloc (EInt (-c)) }
  | LPAREN e = expr RPAREN
      { e }

(* TYPE EXPRESSIONS *)

(* simple_type_expr:
 *       | c=LID { mk_type_expr $sloc (Typeconstr (c,[])) }
 *       | QUOTE v=LID { mk_type_expr $sloc (Typevar v) }
 * 
 * type_expr:
 *       | t=simple_type_expr { t }
 *       (\* | t=simple_type_expr c=IDENT { mk_type_expr $sloc (Typeconstr (c,[t])) } *\) *)
                       
