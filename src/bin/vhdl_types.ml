(* VHDL types and values *)

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
    
let type_name t = Types.string_of_type t

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

let to_string_fn t s = match t with
  | Unsigned n -> Printf.sprintf "integer'image(integer(%s))" s
  | Signed n -> Printf.sprintf "integer'image(integer(%s))" s
  | Integer _ -> Printf.sprintf "integer'image(%s)" s
  | Std_logic -> Printf.sprintf "std_logic'image(%s)" s
  | Variant vd -> Printf.sprintf "%s_to_string(heap,%s)" vd.vd_name s             
  | _ -> "???"

type type_mark = TM_Full | TM_Abbr | TM_None [@@warning "-37"]

let rec string_of_vhdl_type ?(type_marks=TM_Full) t = match t, type_marks with 
  | Unsigned n, TM_Full -> Printf.sprintf "unsigned(%d downto 0)" (n-1)
  | Unsigned n, TM_Abbr -> Printf.sprintf "unsigned%d" n
  | Unsigned _, TM_None -> "unsigned"
  | Signed n, TM_Full -> Printf.sprintf "signed(%d downto 0)" (n-1)
  | Signed n, TM_Abbr -> Printf.sprintf "signed%d" n
  | Signed _, TM_None -> "signed"
  | Integer (Some (lo,hi)), TM_Full -> Printf.sprintf "integer range %d to %d" lo hi
  | Integer _, _ -> "integer"
  | Std_logic, _ -> "std_logic"
  | Real, _ -> "real"
  | Array (sz,t'), _ -> string_of_vhdl_array_type (sz,t') 
  | Variant vd, _ -> String.map (function ' ' -> '_' | c -> c) vd.vd_name
  | Tuple ts, _ -> Misc.string_of_list (string_of_vhdl_type ~type_marks) "," ts
  | Litteral s, _ -> s
  | NoType, _ -> "<unknown>"

and string_of_type ?(type_marks=TM_Full) t =
  string_of_vhdl_type ~type_marks:type_marks (vhdl_type_of t)

and string_of_vhdl_array_type (sz,t) = 
  Printf.sprintf "%s_arr%d" (string_of_vhdl_array_subtype t) sz

and string_of_vhdl_array_subtype t = match t with 
  | Unsigned n -> "u" ^ string_of_int n
  | Signed n -> "s" ^ string_of_int n
  | Integer _ -> "int"
  | Std_logic -> "sl"
  | Real -> "r"
  | _ -> failwith ("VHDL backend: illegal subtype: " ^ string_of_vhdl_type t)

let value_injector t v = match t with
  | Unsigned _
  | Signed _
  | Integer _ -> Printf.sprintf "val_int(%s)" v
  | Std_logic -> Printf.sprintf "val_bool(%s)" v
  | Variant _ -> v
  | _ -> failwith ("Vhdl.value_injector: " ^ (string_of_vhdl_type t))

let value_extractor t v = match t with
  | Unsigned _
  | Signed _
  | Integer _ -> Printf.sprintf "int_val(%s)" v
  | Std_logic -> Printf.sprintf "bool_val(%s)" v
  | Variant _ -> v
  | _ -> failwith ("Vhdl.value_extractor: " ^ (string_of_vhdl_type t))

let default_value t = match t with
  | Unsigned sz -> Printf.sprintf "to_unsigned(0,%d)" sz
  | Signed sz -> Printf.sprintf "to_signed(0,%d)" sz
  | Integer None -> "0"
  | Integer (Some (lo,hi)) -> string_of_int lo
  | Std_logic -> "'0'"
  | Real -> "0.0"
  | Variant _ -> "val_int(0)"
  | _ -> failwith ("Vhdl.default_value: no default for type " ^ (string_of_vhdl_type t))

let from_std_logic_vector (t:vhdl_type) v =
  let open Printf in
  match t with
  | Unsigned n -> sprintf "unsigned(%s)" v
  | Signed n -> sprintf "signed(%s)" v
  | Integer _ -> sprintf "to_integer(signed(%s))" v 
  | Std_logic -> "v(0)"
  | _ -> failwith "Vhdl.from_std_logic_vector"

let string_of_variant_ctor_desc vc = 
  match vc.vc_arity with
  | 0 -> vc.vc_name
  | n -> vc.vc_name ^ "(" ^ Misc.string_of_list (fun va -> string_of_vhdl_type va.va_typ) "," vc.vc_args ^ ")"

let to_string_fn t s = match t with
  | Unsigned n -> Printf.sprintf "integer'image(integer(%s))" s
  | Signed n -> Printf.sprintf "integer'image(integer(%s))" s
  | Integer _ -> Printf.sprintf "integer'image(%s)" s
  | Std_logic -> Printf.sprintf "std_logic'image(%s)" s
  | Variant vd -> Printf.sprintf "%s_to_string(heap,%s)" vd.vd_name s             
  | _ -> "???"
