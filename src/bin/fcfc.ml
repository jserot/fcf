open Fcf
open Syntax
open Printf

let usage = "usage: fcfc [options...] files"

let source_file = ref "<no source_file>"

let anonymous fname = source_file := fname

type mode = Nothing | Run | Dot | Show | Vhdl
let mode = ref Nothing
let dump_typed = ref false
let dump_tenv = ref false
let dump_fsm = ref false
let sopc_dir = ref ""

let options = [
  "-run", Arg.Unit (fun _ -> mode := Run), "run program";
  "-dump_typed", Arg.Unit (fun _ -> dump_typed := true), "dump typed FSM";
  "-dump_tenv", Arg.Unit (fun _ -> dump_tenv := true), "dump builtin typing environment";
  "-dump_fsm", Arg.Unit (fun _ -> dump_fsm := true), "dump FSM intermediate representation";
  "-trace", Arg.Unit (fun _ -> Eval.trace := true), "trace execution when running program";
  "-dot", Arg.Unit (fun _ -> mode := Dot), "generate dot representation";
  "-show", Arg.Unit (fun _ -> mode := Show), "generate and view dot representation";
  "-vhdl", Arg.Unit (fun _ -> mode := Vhdl), "generate VHDL code";
  "-vhdl_numeric_std", Arg.Unit (fun _ -> Vhdl.cfg.use_numeric_std <- true), "translate integers as numeric_std [un]signed (default: false)";
  "-vhdl_default_int_size", Arg.Int (fun s -> Vhdl.cfg.default_int_size <- s), "default int/signed/unsigned size when not specified";
  "-sopc", Arg.String (fun d -> sopc_dir := d; Vhdl.cfg.use_support_lib <- false),
    "generate SOPC files to be used by QSys and Quartus in the specified dir (default: don't)";
]

let parse fname = 
  let ic = open_in_bin fname in
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  Parser.program Lexer.main !Location.input_lexbuf 

let dump_vhdl ~has_globals (n,f) =
  let m = f.fd_desc |> Fsm.from_ast in
  if !sopc_dir <> "" then 
    begin
      Utils.check_dir ~strict:true !sopc_dir;
      Vhdl.write ~dir:(Utils.subdir !sopc_dir "ip") ~has_globals ~prefix:n m;
      Qsys.write ~dir:!sopc_dir ~prefix:n ~src_file:(!source_file) m
    end
  else
    Vhdl.write ~dir:"." ~has_globals ~prefix:n m

let dump_vhdl_globals typed_consts consts =
  if !sopc_dir <> "" then 
    begin
      Utils.check_dir ~strict:true !sopc_dir;
      Vhdl.write_globals ~dir:(Utils.subdir !sopc_dir "ip") ~fname:"globals.vhd" typed_consts consts
    end
  else
    Vhdl.write_globals ~dir:"." ~fname:"globals.vhd" typed_consts consts

let compile name =
  if !dump_tenv then Typing.dump_typing_environment (snd Builtins.typing_env);
  let p = parse !source_file in
  let tp = Typing.type_program Builtins.typing_env p in
  if !dump_typed then Typing.dump_typed_program tp;
  if !dump_fsm then List.iter (fun (_,f) -> f.fd_desc |> Fsm.from_ast |> Fsm.dump) p.p_fsms;
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
     let has_globals = p.p_consts <> [] in
     if has_globals then dump_vhdl_globals tp.tp_consts p.p_consts;
     List.iter (dump_vhdl ~has_globals:has_globals) p.p_fsms
  | Nothing -> ()


let main () =
try
  Sys.catch_break true;
  Arg.parse options anonymous usage;
  compile ()
with
  | Parser.Error -> Error.syntax_error (); exit 1
  | Lexer.Illegal_character (pos,c) -> Error.illegal_char pos c; exit 2
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
