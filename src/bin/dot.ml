open Fcf

type options = {
    mutable node_shape: string;
    mutable node_style: string;
    mutable rankdir: string;
    mutable layout: string;
    mutable mindist: float
  }

let default_options = {
    node_shape = "circle";
    node_style = "solid";
    rankdir = "UD";
    layout = "dot"; 
    mindist = 1.0;
  }

let output oc ?(options=default_options) m = 
    let open Fsm in
    let ini_id = "_ini" in
    let dump_istate () = 
      Printf.fprintf oc "%s [shape=point; label=\"\"; style = invis]\n" ini_id in
    let dump_state id =
      Printf.fprintf oc "%s [label = \"%s\", shape = %s, style = %s]\n"
        id
        id
        options.node_shape
        options.node_style in
    let string_of_guards gs = 
      let s = Misc.string_of_list Syntax.string_of_guard "," gs in 
      s, String.length s in
    let string_of_actions actions = 
      let ss = List.map Action.to_string actions in
      let l = List.fold_left (fun m s -> max m (String.length s)) 0 ss in
      let s = Misc.string_of_list Fun.id "\\n" ss in
      s, l in
    let dump_itransition (dst,actions) =
      let s, l  = string_of_actions actions in
      match s with
      | "" ->
         Printf.fprintf oc "%s->%s\n" ini_id dst
      | _ ->
         let sep = "\n" ^ String.make l '_' ^ "\n" in
         Printf.fprintf oc "%s->%s [label=\"%s%s\"]\n" ini_id dst sep s in
    let dump_transition (src,guards,actions,dst) =
      let s1, l1  = string_of_guards guards in
      let s2, l2  = string_of_actions actions in
      match s1, s2 with 
      | "", "" ->
         Printf.fprintf oc "%s->%s\n" src dst
      | _, "" ->
         Printf.fprintf oc "%s->%s [label=\"%s\"]\n" src dst s1
      | "", _ ->
         let sep = "\n" ^ String.make l2 '_' ^ "\n" in
         Printf.fprintf oc "%s->%s [label=\"%s%s\"]\n" src dst sep s2
      | _, _ ->
         let sep = "\n" ^ String.make (max l1 l2) '_' ^ "\n" in
         Printf.fprintf oc "%s->%s [label=\"%s%s%s\"]\n" src dst s1 sep s2 in
    Printf.fprintf oc "digraph %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n"
      m.m_name
      options.layout
      options.rankdir
      options.mindist;
    dump_istate ();
    List.iter dump_state m.m_states;
    dump_itransition m.m_itrans;
    List.iter dump_transition m.m_trans;
    Printf.fprintf oc "}\n"

let write fname ?(options=default_options) m = 
  let oc = open_out fname in
  output oc ~options m;
  Printf.printf "Wrote file %s\n" fname;
  close_out oc
  
let view ?(options=default_options) ?(fname="") ?(cmd="open -a Graphviz") m = 
  let fname = match fname with
    | "" -> "/tmp/" ^ m.Fsm.m_name ^ "_fsm.dot"
    | _ -> fname in
  let _ = write fname ~options m in
  Sys.command (cmd ^ " " ^ fname)
