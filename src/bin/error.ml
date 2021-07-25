open Fcf
open Location
open Printf
(* open Misc *)
open Pr_type

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

let wrong_guard_type typ loc =
  eprintf "%aThis guard expression has type %s; it should have type bool.\n" 
    output_location loc (string_of_type typ);
  flush stderr

let wrong_type site ty1 ty2 loc =
  eprintf "%aAn error occured when typing this %s : types '%s' and '%s' cannot be unified.\n"
    output_location loc
    site
    (string_of_type ty1)
    (string_of_type ty2);
  flush stderr

let circular_type site ty1 ty2 loc =
  eprintf "%aAn error occured when typing this %s : a cycle was detected between types %s and %s.\n"
    output_location loc
    site
    (string_of_type ty1)
    (string_of_type ty2);
  flush stderr
