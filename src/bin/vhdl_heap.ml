(* Auxilliary fns for manipulating an (abstract) heap of allocated value.
   To be used by the VHDL module *)

open Fcf
open Vhdl_types

type heap_ptr = int

type imm_value =
  Int of int
| Bool of bool
        
type value = Imm of imm_value | Ptr of heap_ptr

type word =
  | Header of int * int (* tag, block_size *)
  | Value of value
  | Empty

type t = word Array.t
         

let make sz = Array.make sz Empty

let rec alloc variants heap h_ptr (e:Syntax.expr) =
  let open Syntax in
  match e.e_desc,vhdl_type_of (e.e_typ) with
  | EInt i, Unsigned _
  | EInt i, Signed _
  | EInt i, Integer _ -> Imm (Int i), h_ptr (* no alloc *)
  | EBool b, Std_logic -> Imm (Bool b), h_ptr (* no alloc *)
  | ECon0 c, Variant vd -> 
     let vd' = lookup_variant_desc vd.vd_name variants in
     let vc = lookup_variant_ctor c vd'.vd_ctors in 
     Imm (Int vc.vc_tag), h_ptr  (* no alloc: nullary ctors are represented by their tag *)
  | ECon1 (c,args), Variant vd -> 
     let args' = begin match args.Syntax.e_desc with Syntax.ETuple es -> es | _ -> [args] end in
     let vd' = lookup_variant_desc vd.vd_name variants in
     let vc = lookup_variant_ctor c vd'.vd_ctors in 
     heap.(h_ptr) <- Header (vc.vc_tag, vc.vc_arity);
     let h_ptr' = 
       Misc.list_fold_lefti  
         (fun i h_ptr' arg ->
           let v, h_ptr'' = alloc variants heap (h_ptr+vc.vc_arity+1) arg in 
           heap.(h_ptr+i+1) <- Value v;
           h_ptr'')
         h_ptr
       args' in
     Ptr h_ptr, h_ptr'
  | _, _ -> failwith ("Heap.alloc: illegal value: " ^ Syntax.string_of_expr e ^ ": " ^ Types.string_of_type e.e_typ)

(* let rec decode_list_value v = match v with
 *     | Imm 0 -> Nil
 *     | Imm _ -> failwith "decode_list_value"
 *     | Ptr addr ->
 *        match heap.(addr), heap.(addr+1), heap.(addr+2) with
 *        | Header (TCons, 2), Value (Imm x), Value v' -> Cons (x, decode_list_value v')
 *        | _, _, _ -> failwith "decode_list_value" *)

(* let rec decode_tree_value v = match v with
 *     | Imm 0 -> Nul
 *     | Imm _ -> failwith "decode_tree_value"
 *     | Ptr addr ->
 *        match heap.(addr), heap.(addr+1), heap.(addr+2), heap.(addr+3) with
 *        | Header (TNode, 3), Value (Imm x), Value v', Value v'' -> Node (x, decode_tree_value v', decode_tree_value v'')
 *        | _, _, _, _ -> failwith "decode_tree_value" *)

let string_of_value v = match v with
  | Imm (Int x) -> Printf.sprintf "val_int(%d)" x
  | Imm (Bool x) -> Printf.sprintf "val_bool(%b)" x
  | Ptr p -> Printf.sprintf "val_ptr(%d)" p

let string_of_word w = match w with
  | Header (tag,sz) -> Printf.sprintf "mk_header(%d,%d)" tag sz
  | Value v -> string_of_value v
  | Empty -> failwith "Heap.string_of_word: empty word"
