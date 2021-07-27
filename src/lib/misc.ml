exception Error

let fatal_error msg = failwith msg

let string_of_list f sep l =
  let rec h = function [] -> "" | [x] -> f x | x::xs -> f x ^ sep ^ h xs in
  h l

let rec list_iter3 f l1 l2 l3 = 
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; list_iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "Misc.list_iter3"

let rec pow2 k = if k = 0 then 1 else 2 * pow2 (k-1)
