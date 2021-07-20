let string_of_list f sep l =
  let rec h = function [] -> "" | [x] -> f x | x::xs -> f x ^ sep ^ h xs in
  h l

(* let check_dir path = 
 *   if not (Sys.file_exists path && Sys.is_directory path)
 *   then Unix.mkdir path 0o777 *)

(* let check_dir path = 
 *   if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory")) *)

