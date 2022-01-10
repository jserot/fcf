exception Error

let fatal_error msg = failwith msg

let string_of_list ?(max_elems=16) f sep l =
  let rec h i l = match l with
    | [] -> ""
    | [x] -> f x
    | x::xs ->
       let rest = if i < max_elems then h (i+1) xs else "..." in
       f x ^ sep ^ rest in
  h 1 l

let rec list_iter3 f l1 l2 l3 = 
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; list_iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "Misc.list_iter3"

let rec pow2 k = if k = 0 then 1 else 2 * pow2 (k-1)

let bits_of_int sz n = 
  let b = Bytes.create sz in 
  let rec h n i =
    if i>=0 then begin
      Bytes.set b i (if n mod 2 = 1 then '1' else '0');
      h (n/2) (i-1)
      end in
  h n (sz-1);
  Bytes.to_string b
