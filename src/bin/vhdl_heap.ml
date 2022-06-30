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

let string_of_value = function   (* for debug only *)
| Imm (Int v) -> "Imm<" ^ string_of_int v ^ ">"
| Imm (Bool v) -> "Imm<" ^ string_of_bool v ^ ">"
| Imm (Float v) -> "Imm<" ^ string_of_float v ^ ">"
| Ptr p -> "Ptr<" ^ string_of_int p ^ ">"

let rec alloc ?(depth=2) variants heap h_ptr (e:Syntax.expr) =
  (* let tab sz = String.make sz '*' in (\* for debug only *\) *)
  let open Syntax in
  match e.e_desc,vhdl_type_of (e.e_typ) with
  | EInt i, Unsigned _
  | EInt i, Signed _
  | EInt i, Integer _ -> h_ptr, Imm (Int i) (* no alloc *)
  | EBool b, Std_logic -> h_ptr, Imm (Bool b) (* no alloc *)
  | EFloat f, Real -> h_ptr, Imm (Float f) (* no alloc *)
  | ECon0 c, Variant vd -> 
     let vd' = lookup_variant_desc vd.vd_name variants in
     let vc = lookup_variant_ctor c vd'.vd_ctors in 
     h_ptr, Imm (Int vc.vc_tag)  (* no alloc: nullary ctors are represented by their tag *)
  | ECon1 (c,args), Variant vd -> 
     (try
       (* Printf.printf "%s Vhdl_heap.alloc(%s, h_ptr=%d)\n" (tab depth) (Syntax.string_of_expr e) h_ptr; *)
       let vd' = lookup_variant_desc vd.vd_name variants in
       let vc = lookup_variant_ctor c vd'.vd_ctors in 
       let args' = begin match args.Syntax.e_desc with Syntax.ETuple es -> es | _ -> [args] end in
       (* Allocate arguments first ... *)
       let h_ptr', v_args = 
         List.fold_left_map  
           (fun h_ptr arg ->
             let h_ptr', v = alloc ~depth:(depth+2) variants heap h_ptr arg in 
             h_ptr', v)
           h_ptr
           args' in
       (* Printf.printf "%s values=[%s] h_ptr'=%d\n" (tab depth) (Misc.string_of_list string_of_value "," v_args) h_ptr'; *)
       (* ... then the block header *)
       heap.(h_ptr') <- Header (vc.vc_tag, vc.vc_arity);
       (* Printf.printf "%s heap[%d]=<%d,%d>\n" (tab depth) h_ptr' vc.vc_tag vc.vc_arity; *)
       List.iteri
         (fun i v ->
           heap.(h_ptr'+i+1) <- Value v
           (* Printf.printf "%s heap[%d]=%s\n" (tab depth) (h_ptr'+i+1) (string_of_value v) *)
           )
         v_args;
       h_ptr'+vc.vc_arity+1, Ptr h_ptr'
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
