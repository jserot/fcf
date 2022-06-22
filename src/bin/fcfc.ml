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

let options = [
  "-run", Arg.Unit (fun _ -> mode := Run), "run program";
  "-dump_typed", Arg.Unit (fun _ -> dump_typed := true), "dump typed FSM";
  "-dump_tenv", Arg.Unit (fun _ -> dump_tenv := true), "dump builtin typing environment";
  "-dump_fsm", Arg.Unit (fun _ -> dump_fsm := true), "dump FSM intermediate representation";
  "-trace", Arg.Unit (fun _ -> Eval.trace := true), "trace execution when running program";
  "-array_print_length", Arg.Int (fun n -> Syntax.array_print_length := n), "max number of printed elements for arrays (default: 32)";
  "-dot", Arg.Unit (fun _ -> mode := Dot), "generate dot representation";
  "-show", Arg.Unit (fun _ -> mode := Show), "generate and view dot representation";
  "-vhdl", Arg.Unit (fun _ -> mode := Vhdl), "generate VHDL code";
  "-vhdl_quartus", Arg.String Vhdl.set_quartus_target, "generate Quartus project ready for synthesis in the specified dir (default: don't)";
  "-vhdl_top", Arg.String (fun s -> Vhdl.cfg.top <- s), "set VHDL toplevel name (for Quartus projects) (default: first FSM name)";
  "-vhdl_qsys", Arg.String Vhdl.set_sopc_target, "generate SOPC files to be used by QSys and Quartus in the specified dir (default: don't)";
  "-vhdl_testbench", Arg.Unit (fun _ -> Vhdl.cfg.with_testbench <- true), "generate VHDL testbench (default: false)";
  "-vhdl_heap_size", Arg.Int (fun s -> Vhdl.cfg.heap_size <- s), "set size of local VHDL heaps (default: 16)";
  "-vhdl_print_heap_size", Arg.Unit (fun _ -> Vhdl.cfg.print_heap_size <- true), "print final heap size after each computation (default: false)";
  "-vhdl_trace_heap", Arg.Unit (fun _ -> Vhdl.cfg.trace_heap <- true), "trace heap contents (default: false)";
]

let parse fname = 
  let ic = open_in_bin fname in
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  Parser.program Lexer.main !Location.input_lexbuf 

let dump_vhdl_fsm ~pkgs (n,f) =
  let m = Fsm.from_ast ~rename_svars:true f.fd_desc in
  match Vhdl.cfg.target with
  | Vhdl.Flat ->
     Vhdl.write_fsm ~dir:"." ~pkgs ~prefix:n m
  | Vhdl.Quartus dir ->
      Vhdl.write_fsm ~dir:dir ~pkgs ~prefix:n m
  | Vhdl.Sopc dir ->
      let m' = Vhdl.write_fsm ~dir:(Utils.subdir dir "ip") ~pkgs ~prefix:n m in
      Qsys.write ~dir:dir ~prefix:n ~src_file:(!source_file) m;
      m'

let dump_vhdl_globals typed_pgm pgm =
  match Vhdl.cfg.target with
  | Vhdl.Flat ->
     Vhdl.write_globals ~dir:"." ~fname:"globals.vhd" typed_pgm pgm
  | Vhdl.Quartus dir ->
     Vhdl.write_globals ~dir:dir ~fname:"globals.vhd" typed_pgm pgm
  | Vhdl.Sopc dir ->
      Utils.check_dir ~strict:true dir;
      Vhdl.write_globals ~dir:(Utils.subdir dir "ip") ~fname:"globals.vhd" typed_pgm pgm

let compile () =
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
     begin match Vhdl.cfg.target with
     | Vhdl.Flat -> ()
     | Vhdl.Quartus dir -> 
         Vhdl.cfg.with_testbench <- false;  (* Override flags since they are meaningful in this case *)
         Vhdl.cfg.trace_heap <- false; 
         Vhdl.cfg.print_heap_size <- false;
         Utils.check_dir ~strict:false dir  (* Subdir will be created if necessary *)
     | Vhdl.Sopc dir -> 
         Vhdl.cfg.with_testbench <- false;  (* Override flags since they are meaningful in this case *)
         Vhdl.cfg.trace_heap <- false; 
         Vhdl.cfg.print_heap_size <- false;
         Utils.check_dir ~strict:true dir   (* Subdir must exist in this case *)
     end;
     let used_packages, ud_types = dump_vhdl_globals tp p in
     let models = List.map (dump_vhdl_fsm ~pkgs:used_packages) p.p_fsms in
     if Vhdl.cfg.with_testbench && Vhdl.cfg.target = Vhdl.Flat then
       Vhdl.write_testbench ~dir:"." ~fname:"tb.vhd" ~pkgs:used_packages ~variants:ud_types models p.p_insts;
     begin match Vhdl.cfg.target with
     | Vhdl.Quartus dir ->
        let mnames = List.map fst models @ ["globals"; "utils"; "values"]  in
        let top =
          begin match Vhdl.cfg.top with
          | "" -> List.hd mnames
          | t -> t 
          end in
        Quartus.write_qsf ~dir:dir top mnames;
        Quartus.write_qpf ~dir:dir top 
     | _ -> ()
     end
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
  | Vhdl.Heap_full fsm -> Error.vhdl_heap_full fsm
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
