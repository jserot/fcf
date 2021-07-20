open Fcf
(* open Printf *)

let usage = "usage: ffsmc [options...] files"

let source_file = ref "<no source_file>"

let anonymous fname = source_file := fname

type mode = Nothing | Run | Dot | Show | Vhdl

let mode = ref Nothing

let options = [
  "-run", Arg.Unit (fun _ -> mode := Run), "run program";
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
  match !mode with
  | Run ->
     let results = Eval.eval_program p in
     List.iter (fun r -> Printf.printf "-: ? = %s\n" (Value.to_string r)) results
  | Dot ->
     List.iter
       (fun (n,f) -> f |> Fsm.from_ast |> Dot.write (n ^ ".dot"))
       p.p_fsms
  | Show ->
     List.iter
       (fun (n,f) -> f |> Fsm.from_ast |> Dot.view |> ignore)
       p.p_fsms
  | Vhdl ->
     List.iter
       (fun (n,f) -> f |> Fsm.from_ast |> Vhdl.write ~dir:"." ~prefix:n)
       p.p_fsms
  | Nothing -> ()

let main () =
(* try *)
  Sys.catch_break true;
  Arg.parse options anonymous usage;
  compile ()
(* with
 * | e -> Error.handle e *)

let _ = Printexc.print main ()
