open Fcf
open Location
open Printf
(* open Misc *)
(* open Pr_type *)

let no_loc = no_location

let syntax_error () = 
  let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
  let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
  eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
  flush stderr

let illegal_char pos c = 
  let l = Loc(!Location.input_name,pos, pos+1) in
  eprintf "%aIllegal character.\n" output_location l;
  flush stderr

let unbound_value name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_string name;
  flush stderr
