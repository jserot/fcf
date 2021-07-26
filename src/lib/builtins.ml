open Types

let type_arithm () = 
  let sg = make_var () in
  let sz = make_var () in
  { ts_params = { tp_typ=[]; tp_sign=[sg]; tp_size=[sz] };
    ts_body = type_arrow
                (type_pair (TyInt (Var sg, Var sz)) (TyInt ( Var sg, Var sz)))
                (TyInt (Var sg, Var sz)) }

and type_compare () = 
  let sg = make_var () in
  let sz = make_var () in
  { ts_params = { tp_typ=[]; tp_sign=[sg]; tp_size=[sz] };
    ts_body = type_arrow
                (type_pair (TyInt (Var sg, Var sz)) (TyInt (Var sg, Var sz)))
                TyBool }

let decode_int = function Value.Int v -> v | _ -> failwith "decode_int"
let decode_bool = function Value.Bool v -> v | _ -> failwith "decode_Bool"
                                              
let prim2_bool f args = match args with
  | [arg1; arg2] -> Value.Bool (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let prim2_int f args = match args with
  | [arg1; arg2] -> Value.Int (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let primitives = [
    "=", (type_compare (), prim2_bool ( = ));
    "<", (type_compare (), prim2_bool ( < ));
    ">", (type_compare (), prim2_bool ( > ));
    "<=", (type_compare (), prim2_bool ( <= ));
    ">=", (type_compare (), prim2_bool ( >= ));
    "!=", (type_compare (), prim2_bool ( != ));
    "+", (type_arithm (), prim2_int ( + ));
    "-", (type_arithm (), prim2_int ( - ));
    "*", (type_arithm (), prim2_int ( * ));
    "/", (type_arithm (), prim2_int ( / ));
  ]

let typing_env =
 [ "int", 0;
   "bool", 0; ],
  List.map
    (fun (id,(ty,_)) -> id, ty)
    primitives

let eval_env = List.map (fun (id, (_,f)) -> id, f) primitives
