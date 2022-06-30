exception Error

let fatal_error msg = failwith msg

let list_fold_lefti f acc l = 
  let rec fold i acc l = match l with
    | [] -> acc
    | x::xs -> fold (i+1) (f i acc x) xs in
  fold 0 acc l 

let list_find_opt2 (f:'a->bool*'b) (l:'a list) = 
  let rec find l = match l with
    | [] -> None
    | x::xs -> (match f x with true, r -> Some r | false, _ -> find xs) in
  find l

let rec list_iter3 f l1 l2 l3 = 
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; list_iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "Misc.list_iter3"

let list_unique ~eq l =
  let rec h acc l = match l with
    | [] -> List.rev acc
    | x::xs ->
       let acc' = if List.exists (eq x) acc then acc else x::acc in
       h acc' xs in
  h [] l

let string_of_list ?(max_elems=max_int) f sep l =
  let rec h i l = match l with
    | [] -> ""
    | [x] -> f x
    | x::xs ->
       let rest = if i < max_elems then h (i+1) xs else "..." in
       f x ^ sep ^ rest in
  h 1 l

let string_of_indexed_list ?(max_elems=max_int) f sep l =
  let rec h i l = match l with
    | [] -> ""
    | [x] -> f i x
    | x::xs ->
       let rest = if i < max_elems then h (i+1) xs else "..." in
       f i x ^ sep ^ rest in
  h 1 l

let string_of_array ?(max_elems=max_int) f sep a = string_of_list ~max_elems f sep (Array.to_list a)

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

let log2 x = log x /. log 2.

let bits_from_card n = int_of_float (ceil (log2 (float_of_int n)))

let quote q s = q ^ s ^ q

