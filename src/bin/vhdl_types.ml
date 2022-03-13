open Fcf

type vhdl_type = 
  | Unsigned of int
  | Signed of int
  | Integer of int_range option
  | Std_logic
  | Real
  | NoType  (* for debug only *)
  | Tuple of vhdl_type list (* for testbench arguments *)
  | Array of int * vhdl_type
  | Variant of variant_desc
  | Litteral of string             

and int_range = int * int (* lo, hi *)             

and variant_desc = 
   { vd_name: string;            (* full type name *)
     vd_ctors: variant_ctor_desc list }    (* value ctors *)

and variant_ctor_desc =
  { vc_name: string;
    vc_tag: int;                       (* 0, 1, ... *)
    vc_arity: int;                     (* Number of args (0 for constant ctors) *)
    vc_args: variant_ctor_arg list; }  (* Arguments  *)

and variant_ctor_arg = 
  { va_idx: int;  (* 1, 2, ... *)
    va_typ: vhdl_type; }

let lookup_variant_desc ty_name variants =
  let rec lookup l = match l with
    | [] -> failwith "Vhdl.lookup_variant_desc"
    | (Variant vd)::rest -> if ty_name = vd.vd_name then vd else lookup rest
    | _::rest -> lookup rest in
  lookup variants

let lookup_variant_ctor c ctors =
  let rec lookup l = match l with
    | [] -> failwith "Vhdl.lookup_variant" 
    | vc::rest -> if c = vc.vc_name then vc else lookup rest in
  lookup ctors
    
let type_name t = String.map (function ' ' -> '_' | c -> c) @@ Types.string_of_type t

let rec vhdl_type_of t =
  let open Types in
  match real_type t with 
  | TyBool -> Std_logic
  | TyFloat -> Real
  | TyInt (Const Unsigned, Const sz) -> Unsigned sz
  | TyInt (Const Signed, Const sz) -> Signed sz
  | TyInt (_, Const sz) -> Signed sz  (* [int<n>] is interpreted as [signed<n>] *)
  | TyInt (_, _) -> Integer None
  | TyProduct [] -> NoType                   
  | TyProduct ts -> Tuple (List.map vhdl_type_of ts)
  | TyArr (Const sz, t') when is_scalar_type t' -> Array (sz, vhdl_type_of t') 
  | TyCon (name, ts) as t' -> Variant { vd_name=type_name t'; vd_ctors=[] }
  | TyAdhoc s -> Litteral s
  | t -> failwith ("VHDL backend: illegal type: " ^ Types.string_of_type t)

and mk_variant_type_desc (* tp *) (t,tc) = 
  let open Types in
  match tc with
    Variant_type ([], cdescs) ->
     (* The set of type variables must be empty since we are dealing with fully instanciated type descriptions *)
     Variant {
         vd_name = type_name t;
         vd_ctors = List.mapi mk_variant_ctor_desc cdescs }
  | _ -> failwith "Vhdl.mk_variant_type_desc"  (* should not happen *)

and mk_variant_ctor_desc i c =
  let open Types in
  let args = match c.cs_arity, c.cs_arg with
    | 0, _  -> []
    | 1, t -> [{ va_idx=1; va_typ=vhdl_type_of t}] 
    | n, TyProduct ts when List.length ts = n -> List.mapi (fun i t -> {va_idx=i+1; va_typ=vhdl_type_of t}) ts
    | _, _ -> failwith "Vhdl.mk_variant_ctor_desc" in
   { vc_name = c.cs_name;
     vc_tag = i;
     vc_arity = c.cs_arity;
     vc_args = args } 

(* and mk_ctor_arg (idx,offset,acc) t = 
 *   let t' = vhdl_type_of t in 
 *   let sz = size_of_simple_vhdl_type t' in
 *   idx+1,
 *   offset+sz,
 *   ({ va_idx = idx;
 *     va_typ = t'
 *     va_size = sz } :: acc) *)

let to_string_fn t s = match t with
  | Unsigned n -> Printf.sprintf "integer'image(integer(%s))" s
  | Signed n -> Printf.sprintf "integer'image(integer(%s))" s
  | Integer _ -> Printf.sprintf "integer'image(%s)" s
  | Std_logic -> Printf.sprintf "std_logic'image(%s)" s
  | Variant _ -> Printf.sprintf "string_of_value(%s)" s             
  | _ -> "???"
