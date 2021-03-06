type t =
  | TyInt of sign attr * size attr
  | TyBool
  | TyFloat
  | TyArrow of t * t
  | TyProduct of t list 
  | TyVar of t var 
  | TyArr of size attr * t
  | TyCon of string * t list
  | TyUnit
  | TyAdhoc of string (* this is a hack to allow backend-specific types to be specified at AST level *)

and 'a attr =
  | Const of 'a
  | Var of ('a attr) var

and 'a var =
  { stamp: string;
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

and sign = Signed | Unsigned
and size = 
  | Width of int (* bits *)
  | Range of int * int (* lo, hi *)
          
type typ_scheme =
  { ts_params: typ_params;
    ts_body: t }

and typ_params = {
  mutable tp_typ: (t var) list;
  mutable tp_sign: ((sign attr) var) list;
  mutable tp_size: ((size attr) var) list;
  }

(* Type and values constructor descriptions *)

type type_desc =
  { ty_arity: int;              (* Arity *)
    ty_desc: type_components;   (* Description *)
    mutable ty_insts: (t * type_components) list;   (* Instances (for polymorphic type ctors) *)
  } 

and type_components =
  | Abstract_type
  | Variant_type of t var list * constr_desc list   

and constr_desc =
  { cs_name: string;
    cs_arity: int;                   (* 0 for constant ctors *)
    (* cs_params: t var list;            *)
    cs_res: t;                       (* Result type *)
    cs_arg: t; }                     (* Argument type *)

(* Builders *)

let new_stamp =
  let var_cnt = ref 0 in
  function () -> incr var_cnt; "_" ^ string_of_int !var_cnt

let make_var () = { value = Unknown; stamp=new_stamp () }

let new_type_var () = make_var ()
let new_attr_var () = make_var ()

let type_int () = TyInt (Var (make_var ()), Var (make_var ()))
let type_arrow t1 t2 = TyArrow (t1, t2)
let type_pair t1 t2 = TyProduct [t1;t2]
let type_array t = TyArr (Var (make_var ()), t)
let type_sized_array sz t = TyArr (Const (Width sz), t)
let type_constr c ts = TyCon (c, ts)
let type_unit = TyUnit
let type_product ts = TyProduct ts

let trivial_scheme t = {
    ts_params={ tp_typ=[]; tp_sign=[]; tp_size=[] };
    ts_body=t }
                     
let no_type = TyProduct []
let no_type_scheme = trivial_scheme no_type

exception Illegal_function_type of t

(* Path compression *)

let rec type_repr = function
  | TyVar ({value = Known ty1; _} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

let rec attr_repr = function
  | Var ({value = Known r1; _} as var) ->
      let r = attr_repr r1 in
      var.value <- Known r;
      r
  | r -> r

let real_attr a = 
  match attr_repr a with
  | Var { value=Known v'; _} -> v'
  | r -> r

let real_type ty = 
  match type_repr ty with
  | TyInt (sg, sz) -> TyInt (real_attr sg, real_attr sz)
  | TyVar { value=Known ty'; _} -> ty'
  | ty -> ty

let is_scalar_type ty = match real_type ty with
| TyInt _ | TyBool | TyFloat -> true
| _ -> false

let is_const_type ty = match real_type ty with
| TyArr (_,t) -> is_scalar_type t
| _ -> is_scalar_type ty

let is_variant_type ty = match real_type ty with
| TyCon (_,_) -> true
| _ -> false

let rec is_ground_type ?(strict=false) ty = 
  match real_type ty with
  | TyInt (_,_) | TyBool | TyFloat -> true
  | TyArrow (t1, t2) -> false
  | TyProduct ts -> List.for_all is_ground_type ts
  | TyArr (sz, t) -> is_ground_type t
  | TyCon (_, ts) -> List.for_all is_ground_type ts
  | TyUnit -> true
  | _ -> false

let list_of_types t = match real_type t with
    | TyProduct ts -> ts
    | _ -> [t] 

let fn_types t =
  match t with
  | TyArrow (t1, t2) -> list_of_types t1, list_of_types t2
  | _ -> raise (Illegal_function_type t)

let rec is_mono_type = function
  | TyArrow (t1, t2) -> is_mono_type t1 && is_mono_type t2
  | TyProduct ts -> List.for_all is_mono_type ts
  | TyCon (c,ts) -> List.for_all is_mono_type ts
  | TyArr (_,t) -> is_mono_type t
  | TyVar ({value = Known ty1; _}) -> is_mono_type ty1
  | TyVar ({value = Unknown; _}) -> false
  | _ -> true

(* Unification *)

exception TypeConflict of t * t
exception TypeCircularity of t * t

let unify_attr (ty1,ty2) a1 a2 =
  let val1 = real_attr a1
  and val2 = real_attr a2 in
  if val1 == val2 then ()
  else
  match (val1, val2) with
    | Const s1, Const s2 when s1 = s2 -> ()
    | Var var1, Var var2 when var1 == var2 -> ()  (* This is hack *)
    | Var var, v -> var.value <- Known v
    | v, Var var -> var.value <- Known v
    | _, _ -> raise (TypeConflict(ty1, ty2))

let rec unify ty1 ty2 =
  let val1 = real_type ty1
  and val2 = real_type ty2 in
  if val1 == val2 then () else
  match (val1, val2) with
  | TyVar v1, TyVar v2 when v1==v2 -> 
      ()
  | TyVar var, ty ->
      occur_check var ty;
      var.value <- Known ty
  | ty, TyVar var ->
      occur_check var ty;
      var.value <- Known ty
  | TyUnit, TyUnit -> ()
  | TyBool, TyBool -> ()
  | TyInt (sg1,sz1), TyInt (sg2,sz2) ->
     unify_attr (val1,val2) sg1 sg2;
     unify_attr (val1,val2) sz1 sz2;
  | TyArrow(ty1, ty2), TyArrow(ty1', ty2') ->
      unify ty1 ty1';
      unify ty2 ty2'
  | TyProduct ts1, TyProduct ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 unify ts1 ts2
  | TyArr (sz1,ty1), TyArr (sz2,ty2) ->
     unify_attr (val1,val2) sz1 sz2;
     unify ty1 ty2;
  | TyCon (c1,ts1), TyCon (c2,ts2) when c1=c2 && List.length ts1 = List.length ts2 ->
      List.iter2 unify ts1 ts2
  | _, _ ->
     raise (TypeConflict(val1, val2))

and occur_check var ty =
  let rec test s =
    match type_repr s with
    | TyVar var' ->
        if var == var' then raise(TypeCircularity(TyVar var,ty))
    | TyArrow (ty1,ty2) ->
        test ty1;
        test ty2
    | TyProduct ts ->
        List.iter test ts
    | TyCon (c, args) ->
        List.iter test args
    | _ ->
        ()
  in test ty

let copy_attr bs a =
  match attr_repr a with
  | Var var as v ->
      begin try
        List.assq var bs 
      with Not_found ->
        v
      end
  | r -> r

type bindings =
  { tb_typ: (t var * t) list;
    tb_sign: ((sign attr) var * sign attr) list;
    tb_size: ((size attr) var * size attr) list; }
  
let copy_type bs ty =
  let rec copy ty = 
    match type_repr ty with
    | TyVar var as ty ->
        begin try
          List.assq var bs.tb_typ
        with Not_found ->
            ty
        end
    | TyInt (sg, sz) ->
       TyInt (copy_attr bs.tb_sign sg, copy_attr bs.tb_size sz)
    | TyArrow (ty1, ty2) ->
       TyArrow (copy ty1, copy ty2)
    | TyProduct ts ->
       TyProduct (List.map copy ts)
    | TyCon (c,ts) ->
       TyCon (c, List.map copy ts)
    | ty -> ty in
  copy ty

let type_instance ts =
  match ts.ts_params with
  | { tp_typ=[]; tp_sign=[]; tp_size=[] } -> ts.ts_body  (* Monotype *)
  | _ ->
     copy_type
       { tb_typ = List.map (fun var -> (var, TyVar (make_var()))) ts.ts_params.tp_typ;
         tb_sign = List.map (fun var -> (var, Var (make_var()))) ts.ts_params.tp_sign;
         tb_size = List.map (fun var -> (var, Var (make_var()))) ts.ts_params.tp_size }
       ts.ts_body

let type_copy t = copy_type { tb_typ=[]; tb_sign=[]; tb_size=[] } t 

(* Generalization *)

let generalize env ty =
  (* Note : we use here a naive version in which generic variables are detected by
     simply checking whether they do not occur free in the englobing typing environment.
     A more efficient version would use binding levels *)
  let vars_of vars' ty = 
     (* Returns the list of variables occuring in [t] but not in [vars'] *)
    let vars = { tp_typ=[]; tp_sign=[]; tp_size=[] } in
    let rec scan_ty t =
      match type_repr t with
      | TyVar var ->
          if not (List.memq var vars.tp_typ) && not (List.memq var vars'.tp_typ) 
          then vars.tp_typ <- var :: vars.tp_typ
      | TyArrow (t1, t2) ->
          scan_ty t1;
          scan_ty t2
      | TyProduct ts ->
          List.iter scan_ty ts
      | TyCon (c,ts) ->
          List.iter scan_ty ts
      | TyInt (sign, size) ->
          scan_sign sign;
          scan_size size
      | _ -> ()
    and scan_sign s =
      match attr_repr s with
      | Var var ->
          if not (List.memq var vars.tp_sign) && not (List.memq var vars'.tp_sign) 
          then vars.tp_sign <- var :: vars.tp_sign
      | _ -> ()
    and scan_size s =
      match attr_repr s with
      | Var var ->
          if not (List.memq var vars.tp_size) && not (List.memq var vars'.tp_size) 
          then vars.tp_size <- var :: vars.tp_size
      | _ -> () in
    scan_ty ty;
    vars in
  let free_vars = 
    let add_vars vs vs' =
    { tp_typ=vs.tp_typ@vs'.tp_typ;
      tp_sign=vs.tp_sign@vs'.tp_sign;
      tp_size=vs.tp_size@vs'.tp_size } in 
    List.fold_left
      (fun vs (_,ts) -> 
        let vs' = vars_of ts.ts_params ts.ts_body in
        add_vars vs vs')
      { tp_typ=[]; tp_sign=[]; tp_size=[] }
      env in
  let gen_vars = vars_of free_vars ty in
  { ts_params =
      { tp_typ = List.rev gen_vars.tp_typ;
        tp_sign = List.rev gen_vars.tp_sign;
        tp_size = List.rev gen_vars.tp_size };
    ts_body = ty }

let rec type_equal t1 t2 =
  match real_type t1, real_type t2 with
  | TyVar { stamp=s1; value=Unknown }, TyVar { stamp=s2; value=Unknown } ->
      s1 = s2
  | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
      type_equal ty1 ty2 && type_equal ty1' ty2'
  | TyProduct ts, TyProduct ts' when List.length ts = List.length ts'->
      List.for_all2 type_equal ts ts'
  | TyInt (sg1, sz1), TyInt(sg2, sz2) ->
      begin match real_attr sg1, real_attr sg2, real_attr sz1, real_attr sz2 with
      | Const sg1, Const sg2, Const sz1, Const sz2 -> sg1=sg2 && sz1=sz2
      | Const sg1, Const sg2, _ , _ -> sg1=sg2
      | _, _ , Const sz1, Const sz2 -> sz1=sz2
      | _, _ , _, _ -> true
      end
  | TyCon (c1, args1), TyCon(c2, args2) when List.length args1 = List.length args2  ->
      c1 = c2 && List.for_all2 type_equal args1 args2
  | _, _ -> 
      false


(* Printing *)

let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (t * string) list)

let reset_type_var_names () =
  type_vars_counter := 0;
  type_vars_names := []

let name_of_type_var var =
  try
    List.assq var !type_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    incr type_vars_counter;
    type_vars_names := (var, name) :: !type_vars_names;
    name

let string_of_sign sg = match real_attr sg with
  | Const Unsigned -> "unsigned"
  | Const Signed -> "signed"
  | _ -> "int"

let string_of_size sz =
  let string_of_sz s = match s with
    | Width s -> string_of_int s
    | Range (lo,hi) -> string_of_int lo ^ ":" ^ string_of_int hi in
  match real_attr sz with
  | Const s -> "<" ^ string_of_sz s ^ ">"
  | _ -> ""

let rec string_of_type t = match real_type t with
  | TyUnit -> "unit"
  | TyBool -> "bool"
  | TyFloat -> "float"
  | TyInt (sg, sz) -> string_of_sign sg ^ string_of_size sz
  | TyArrow (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2
  | TyProduct ts -> "(" ^ Misc.string_of_list string_of_type " * " ts ^ ")"
  | TyCon (c,[]) -> c
  | TyCon (c,ts) ->  Misc.string_of_list string_of_type "_" ts ^ "_" ^ c
  | (TyVar v) as t -> "'" ^ name_of_type_var t
  | TyArr (sz, t') -> string_of_type t' ^ " array" ^ string_of_size sz
  | TyAdhoc s -> "<" ^ s ^ ">"

let string_of_type_scheme ts = 
  reset_type_var_names ();
  string_of_type ts.ts_body

let string_of_type_component tc = match tc with
| Abstract_type -> "<abstract>"
| Variant_type (params,cds) ->
   let string_of_ctor_desc cd = cd.cs_name ^ ":" ^ string_of_type cd.cs_arg ^" -> " ^ string_of_type cd.cs_res in 
   Misc.string_of_list string_of_ctor_desc " | " cds

let string_of_type_inst (t,tc) = "    " ^ string_of_type t ^ ": " ^ string_of_type_component tc
                               
let string_of_type_insts ts = match ts with
  | [] -> ""
  | _ -> " \n  instanciated as:\n" ^ Misc.string_of_list string_of_type_inst "\n" ts ^ ")"

let string_of_type_desc td = 
  string_of_int td.ty_arity ^ ", " ^ string_of_type_component td.ty_desc ^ string_of_type_insts td.ty_insts

let string_of_ctor_desc cd =
  string_of_type cd.cs_arg ^ " -> " ^ string_of_type cd.cs_res
