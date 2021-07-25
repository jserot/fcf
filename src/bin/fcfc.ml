open Fcf
open Syntax
open Printf

let usage = "usage: fcfc [options...] files"

let source_file = ref "<no source_file>"

let anonymous fname = source_file := fname

type mode = Nothing | Run | Dot | Show | Vhdl
let mode = ref Nothing
let dump_typed = ref false

let options = [
  "-run", Arg.Unit (fun _ -> mode := Run), "run program";
  "-dump_typed", Arg.Unit (fun _ -> dump_typed := true), "dump typed FSM";
  "-trace", Arg.Unit (fun _ -> Eval.trace := true), "trace execution when running program";
  "-dot", Arg.Unit (fun _ -> mode := Dot), "generate dot representation";
  "-show", Arg.Unit (fun _ -> mode := Show), "generate and view dot representation";
  "-vhdl", Arg.Unit (fun _ -> mode := Vhdl), "generate VHDL code";
]

let parse fname = 
  let ic = open_in_bin fname in
  (* The source file(s) must be opened in binary mode, so that the absolute seeks in print_location work. *)
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  Parser.program Lexer.main !Location.input_lexbuf 

let compile name =
  let p = parse !source_file in
  let tp = Typing.type_program Builtins.typing_env p in
  if !dump_typed then Typing.dump_typed_program tp;
  match !mode with
  | Run ->
     let results = Eval.eval_program p in
     List.iter (fun r -> printf "-: ? = %s\n" (Value.to_string r)) results
  | Dot ->
     List.iter
       (fun (n,f) -> f.fd_desc |> Fsm.from_ast |> Dot.write (n ^ ".dot"))
       p.p_fsms
  | Show ->
     List.iter
       (fun (n,f) -> f.fd_desc |> Fsm.from_ast |> Dot.view |> ignore)
       p.p_fsms
  | Vhdl ->
     List.iter
       (fun (n,f) -> f.fd_desc |> Fsm.from_ast |> Vhdl.write ~dir:"." ~prefix:n)
       p.p_fsms
  | Nothing -> ()


let main () =
try
  Sys.catch_break true;
  Arg.parse options anonymous usage;
  compile ()
with
  | Parser.Error -> Error.syntax_error (); exit 1
  | Lexer.Illegal_character (pos,c) -> Error.illegal_char pos c; exit 2
  | Typing.Wrong_guard_type(ty,loc) -> Error.wrong_guard_type ty loc
  | Typing.Unbound_value(loc,id) -> Error.unbound_value id loc
  | Typing.Wrong_type(site,loc,ty1,ty2) -> Error.wrong_type site loc ty1 ty2
  | Typing.Circular_type(site,loc,ty1,ty2) -> Error.circular_type site loc ty1 ty2
  | End_of_file -> exit 0
  | Misc.Error -> exit 1
  | Sys.Break -> flush stderr; exit 5
  | Sys_error msg ->
     eprintf "Input/output error: %s.\n" msg;
     flush stderr; exit 6
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 7

let _ = Printexc.print main ()
