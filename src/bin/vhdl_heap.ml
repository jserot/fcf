(* Abstract representation of a heap of allocated values. 
   To be used by the [Vhdl] module *)

open Fcf
open Vhdl_types

type heap_ptr = int

type imm_value =
  Int of int
| Bool of bool
| Float of float
        
type value = Imm of imm_value | Ptr of heap_ptr

type word =
  | Header of int * int (* tag, block_size *)
  | Value of value
  | Empty

type t = word Array.t
         
exception Heap_full
        
let make sz = Array.make sz Empty

let rec alloc variants heap h_ptr (e:Syntax.expr) =
  let open Syntax in
  match e.e_desc,vhdl_type_of (e.e_typ) with
  | EInt i, Unsigned _
  | EInt i, Signed _
  | EInt i, Integer _ -> Imm (Int i), h_ptr (* no alloc *)
  | EBool b, Std_logic -> Imm (Bool b), h_ptr (* no alloc *)
  | EFloat f, Real -> Imm (Float f), h_ptr (* no alloc *)
  | ECon0 c, Variant vd -> 
     let vd' = lookup_variant_desc vd.vd_name variants in
     let vc = lookup_variant_ctor c vd'.vd_ctors in 
     Imm (Int vc.vc_tag), h_ptr  (* no alloc: nullary ctors are represented by their tag *)
  | ECon1 (c,args), Variant vd -> 
     (try
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
     with
       Invalid_argument _ -> raise Heap_full)
  | _, _ -> failwith ("Heap.alloc: illegal value: " ^ Syntax.string_of_expr e ^ ": " ^ Types.string_of_type e.e_typ)

let string_of_value v = match v with
  | Imm (Int x) -> Printf.sprintf "val_int(%d)" x
  | Imm (Bool x) -> Printf.sprintf "val_bool(%b)" x
  | Imm (Float x) -> Printf.sprintf "val_float(%f)" x
  | Ptr p -> Printf.sprintf "val_ptr(%d)" p

let string_of_word w = match w with
  | Header (tag,sz) -> Printf.sprintf "mk_header(%d,%d)" tag sz
  | Value v -> string_of_value v
  | Empty -> failwith "Heap.string_of_word: empty word"
