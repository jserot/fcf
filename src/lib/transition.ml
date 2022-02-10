type t = State.t * Syntax.guard * Action.t list * State.t

let to_string (src,guard,actions,dst) =
  let s0 = src ^ " -> " ^ dst in
  let s1 = Syntax.string_of_guard guard in
  let s2 = Misc.string_of_list Action.to_string "," actions in
  let s3 = match s1, s2 with
    | "", "" -> ""
    | s1, "" -> s1
    | s1, s2 -> s1 ^ "/" ^ s2 in
  match s3 with
    "" -> s0
  | _ -> s0 ^ " [" ^ s3 ^ "]"
