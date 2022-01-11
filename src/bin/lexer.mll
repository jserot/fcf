{
open Parser

exception Illegal_character of int * string

let keyword_table = [
  "let", LET;
  "in", IN;
  "and", AND;
  "return", RETURN;
  "const", CONST;
  "true", TRUE;
  "false", FALSE;
  "int", TYINT;
  "signed", TYSIGNED;
  "unsigned", TYUNSIGNED;
  "bool", TYBOOL;
  "float", TYFLOAT;
  "array", TYARRAY;
]
}

rule main = parse
  | [' ' '\t'] +
      { main lexbuf }
  | ['\010' '\013' ]
      { Lexing.new_line lexbuf; main lexbuf }
  | ['a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme lexbuf  in
        try List.assoc s keyword_table
        with Not_found -> LID s }
  (* | ['A'-'Z' 'a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
   *     { UID (Lexing.lexeme lexbuf) } *)
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "->" { ARROW }
  | "|" { BAR }
  | ":" { COLON }
  (* | "'" { QUOTE } *)
  | "=" { EQUAL }
  | "!="    { NOTEQUAL }
  | '>'    { GT }
  | '<'    { LT }
  | ">="    { GTE }
  | "<="    { LTE }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | "+." { FPLUS }
  | "-." { FMINUS }
  | "*." { FTIMES }
  | "/." { FDIV }
  | eof { EOF }
  | _ { raise (Illegal_character (Lexing.lexeme_start lexbuf, Lexing.lexeme lexbuf)) }
