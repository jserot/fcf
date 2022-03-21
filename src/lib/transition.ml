type t = State.t * Syntax.guard list * Action.t list * State.t

let rename_vars f (s,guards,actions,s') =
  (s,
   List.map (Syntax.rename_guard_vars f) guards,
   List.map (Action.rename_vars f) actions,
   s')

let to_string (src,guards,actions,dst) =
  let s0 = src ^ " -> " ^ dst in
  let s1 = Misc.string_of_list Syntax.string_of_guard "," guards in
  let s2 = Misc.string_of_list Action.to_string "," actions in
  let s3 = match s1, s2 with
    | "", "" -> ""
    | s1, "" -> s1
    | s1, s2 -> s1 ^ "/" ^ s2 in
  match s3 with
    "" -> s0
  | _ -> s0 ^ " [" ^ s3 ^ "]"
