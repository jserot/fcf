{
open Parser

exception Illegal_character of int * string

let keyword_table = [
  "let", LET;
  "in", IN;
  "and", AND;
  "return", RETURN;
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
  (* | "'0'" { BOOL false }
   * | "'1'" { BOOL true } *)
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "->" { ARROW }
  | "|" { BAR }
  (* | ":" { COLON } *)
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
  | eof { EOF }
  | _ { raise (Illegal_character (Lexing.lexeme_start lexbuf, Lexing.lexeme lexbuf)) }
