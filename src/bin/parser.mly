%token LET
%token IN
%token AND
%token RETURN
%token CONST
%token TYPE
%token OF
%token <int> INT
%token <float> FLOAT
%token TRUE
%token FALSE
%token <string> LID
%token <string> UID
%token SEMICOLON
%token BAR
%token TILDE
%token COMMA
%token ARROW
%token COLON
%token QUOTE
%token EQUAL NOTEQUAL
%token FEQUAL FNOTEQUAL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LT GT LTE GTE
%token FLT FGT FLTE FGTE
%token PLUS MINUS TIMES DIV
%token FPLUS FMINUS FTIMES FDIV
%token SHL SHR
%token TYINT
%token TYSIGNED
%token TYUNSIGNED
%token TYBOOL
%token TYFLOAT
%token TYARRAY
%token EOF

(* Precedences and associativities for expressions *)

%left EQUAL NOTEQUAL GT LT GTE LTE
(* %left LAND LOR LXOR *)
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV
%left SHR SHL
(* %nonassoc prec_unary_minus         (\* Highest precedence *\) *)

%start <Syntax.program> program

%{
open Fcf
open Syntax
open Location

let mk_location (p1,p2) =
  let open Lexing in
  Loc (!input_name, p1.pos_cnum, p2.pos_cnum)

let mk_type_expr l desc = { te_desc = desc; te_loc = mk_location l; te_typ = Types.no_type }
let mk_appl l desc = { ap_desc = desc; ap_loc = mk_location l }
let mk_fsm_decl l desc = { fd_desc = desc; fd_loc = mk_location l }
let mk_const_decl l desc = { cst_desc = desc; cst_loc = mk_location l }
let mk_type_decl l desc = { td_desc = desc; td_loc = mk_location l }
let mk_state_decl l desc = { sd_desc = desc; sd_loc = mk_location l; sd_typ = Types.no_type; sd_params = [] }
let mk_transition l desc = { t_desc = desc; t_loc = mk_location l }
let mk_continuation l desc = { ct_desc = desc; ct_loc = mk_location l; ct_typ = Types.no_type }
let mk_expr l desc = { e_desc = desc; e_loc = mk_location l; e_typ = Types.no_type }
let mk_guard l desc = { g_desc = desc; g_loc = mk_location l }
let mk_pat l desc = { p_desc = desc; p_loc = mk_location l }
%}

%%

program:
  | ts=list(type_decl)
    cs=list(const_decl)
    fs=nonempty_list(fsm_decl)
    es=list(fsm_inst) EOF
      { { p_types=ts; p_consts=cs; p_fsms=fs; p_insts=es } }
        
type_decl:
  | TYPE params=type_params name=LID EQUAL td=type_defn SEMICOLON
      { name, params, mk_type_decl $sloc td }

type_params:
        LPAREN params=separated_nonempty_list(COMMA,type_var) RPAREN
          { params }
      | param=type_var
          { [param] }
      | (* empty *)
          { [] }
;

type_var:
        QUOTE id=LID
          { id }
;
type_defn:
  | cds = separated_nonempty_list(BAR, ctor_defn) 
      {  Variant_decl cds }

ctor_defn:
  | c=UID
        { Constr0_decl c }
  | c=UID OF ts=separated_nonempty_list(TIMES,type_expr)
        { Constr1_decl(c,ts) }

const_decl:
  | CONST name=LID COLON t=type_expr EQUAL v=const_expr SEMICOLON
      { name, mk_const_decl $sloc { c_name=name; c_val=v; c_typ=t } }

fsm_decl:
  | LET name=LID ps=params EQUAL ss=states IN s=state_expr SEMICOLON
      { name, mk_fsm_decl $sloc { f_name=name; f_params=ps; f_desc=ss,s; f_typ=Types.no_type_scheme }  }

fsm_inst:
  | f=LID es=args SEMICOLON { mk_appl $sloc (f,es) }

params:
  | (* Nothing *) { [] }
  | LPAREN ps=separated_list(COMMA, param) RPAREN { ps }

param:
  | id=LID t=optional_type_expr { id, t }

optional_type_expr:
  | (* Nothing *) { None }
  | COLON t=type_expr { Some t }

states:
  | LET ss=separated_nonempty_list(AND, state) { ss }

state:
  | name=LID ps=params EQUAL ts=nonempty_list(transition)
     { mk_state_decl $sloc (name, ps, ts) } 

transition:
  | BAR e=guard ARROW c=continuation { mk_transition $sloc (e,c) }

guard:
  | e=expr { mk_guard $sloc (Cond e) }
  | e=expr TILDE pat=pattern { mk_guard $sloc (Match (e,pat)) }

pattern:
  | p=simple_pattern { p }
  | c=UID { mk_pat $sloc (Pat_constr0 c) }
  | c=UID p=simple_pattern { mk_pat $sloc (Pat_constr1 (c,p)) }
  | c=UID LPAREN ps=separated_nonempty_list(COMMA,simple_pattern) RPAREN
      { let p = mk_pat $sloc (Pat_tuple ps) in
        mk_pat $sloc (Pat_constr1 (c,p)) }

simple_pattern:
        v=INT
          { mk_pat $sloc (Pat_int(v)) }
      | MINUS v=INT
          { mk_pat $sloc (Pat_int(-v)) }
      | TRUE
          { mk_pat $sloc (Pat_bool(true)) }
      | FALSE
          { mk_pat $sloc (Pat_bool(false)) }
      | v=LID
          { mk_pat $sloc (Pat_var v) }
        
continuation:
  | RETURN e=ret_expr { mk_continuation $sloc (Return e) }
  | e=state_expr { mk_continuation $sloc (Next e) }

state_expr:
  | s=LID es=args { mk_appl $sloc (s,es) }

args:
  | (* Nothing *) { [] }
  | LPAREN es=separated_list(COMMA,expr) RPAREN { es }

(* EXPRESSIONS *)

ret_expr:
  | e = expr
     { e }
  | LPAREN es=separated_nonempty_list(COMMA,expr) RPAREN
     { match es with 
       | [e] -> e
       | _ -> mk_expr $sloc (ETuple es) } (* The case es=[] is syntactically avoided *)

const_expr:
  | e = scalar_expr 
      { e }
  | LBRACE es=separated_list(COMMA,scalar_expr) RBRACE
      { mk_expr $sloc (EArray es) } 

expr:
  | e = simple_expr
      { e }
  (* | es=expr_comma_list
   *     { mk_expr $sloc (ETuple (List.rev es)) } *)
  | e1 = expr SHL e2 = expr
      { mk_expr $sloc (EBinop ("<<", e1, e2)) }
  | e1 = expr SHR e2 = expr
      { mk_expr $sloc (EBinop (">>", e1, e2)) }
  | e1 = expr PLUS e2 = expr
      { mk_expr $sloc (EBinop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr
      { mk_expr $sloc (EBinop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr
      { mk_expr $sloc (EBinop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr
      { mk_expr $sloc (EBinop ("/", e1, e2)) }
  | e1 = expr FPLUS e2 = expr
      { mk_expr $sloc (EBinop ("+.", e1, e2)) }
  | e1 = expr FMINUS e2 = expr
      { mk_expr $sloc (EBinop ("-.", e1, e2)) }
  | e1 = expr FTIMES e2 = expr
      { mk_expr $sloc (EBinop ("*.", e1, e2)) }
  | e1 = expr FDIV e2 = expr
      { mk_expr $sloc (EBinop ("/.", e1, e2)) }
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
  | e1 = expr FEQUAL e2 = expr
      { mk_expr $sloc (EBinop ("=.", e1, e2)) }
  | e1 = expr FNOTEQUAL e2 = expr
      { mk_expr $sloc (EBinop ("!=.", e1, e2)) }
  | e1 = expr FGT e2 = expr
      { mk_expr $sloc (EBinop (">.", e1, e2)) }
  | e1 = expr FLT e2 = expr
      { mk_expr $sloc (EBinop ("<.", e1, e2)) }
  | e1 = expr FGTE e2 = expr
      { mk_expr $sloc (EBinop (">=.", e1, e2)) }
  | e1 = expr FLTE e2 = expr
      { mk_expr $sloc (EBinop ("<=.", e1, e2)) }

simple_expr:
  | v = LID
      { mk_expr $sloc (EVar v) }
  | e = scalar_expr 
      { e } 
  | LPAREN e = expr RPAREN
      { e }
  | a=LID LBRACKET i=expr RBRACKET
      { mk_expr $sloc (EArrRd (a,i)) } 

scalar_expr:
  | c = INT
      { mk_expr $sloc (EInt c) }
  | c = FLOAT
      { mk_expr $sloc (EFloat c) }
  | TRUE
      { mk_expr $sloc (EBool true) }
  | FALSE
      { mk_expr $sloc (EBool false) }
  | MINUS c=INT
      { mk_expr $sloc (EInt (-c)) }

(* TYPE EXPRESSIONS *)

type_expr:
  | TYINT sz=int_size {  mk_type_expr $sloc (TeInt (None,sz)) }
  | TYSIGNED sz=int_size {  mk_type_expr $sloc (TeInt (Some TeSigned,sz)) }
  | TYUNSIGNED sz=int_size {  mk_type_expr $sloc (TeInt (Some TeUnsigned,sz)) }
  | TYBOOL {  mk_type_expr $sloc TeBool }
  | TYFLOAT {  mk_type_expr $sloc TeFloat }
  | t=type_expr TYARRAY LBRACKET sz=INT RBRACKET { mk_type_expr $sloc (TeArray (sz,t)) }
  | c=LID { mk_type_expr $sloc (TeConstr(c, [])) }
  | t=type_expr c=LID { mk_type_expr $sloc (TeConstr(c, [t])) }
  | LPAREN ts=separated_nonempty_list(COMMA,type_expr) RPAREN c=LID { mk_type_expr $sloc (TeConstr(c, ts)) }
  | v=type_var { mk_type_expr $sloc (TeVar v) }

int_size:
  | (* Nothing *) { None }
  | LT sz=INT GT { Some sz }

