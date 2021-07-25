open Types

let type_arithm = trivial_scheme
  (type_arrow (type_pair type_int type_int) type_int)
and type_compare = trivial_scheme
  (type_arrow (type_pair type_int type_int) type_bool)

let decode_int = function Value.Int v -> v | _ -> failwith "decode_int"
let decode_bool = function Value.Bool v -> v | _ -> failwith "decode_Bool"
                                              
let prim2_bool f args = match args with
  | [arg1; arg2] -> Value.Bool (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let prim2_int f args = match args with
  | [arg1; arg2] -> Value.Int (f (decode_int arg1) (decode_int arg2))
  | _ -> failwith "primitive arity mismatch"

let primitives = [
    "=", (type_compare, prim2_bool ( = ));
    "<", (type_compare, prim2_bool ( < ));
    ">", (type_compare, prim2_bool ( > ));
    "<=", (type_compare, prim2_bool ( <= ));
    ">=", (type_compare, prim2_bool ( >= ));
    "!=", (type_compare, prim2_bool ( != ));
    "+", (type_arithm, prim2_int ( + ));
    "-", (type_arithm, prim2_int ( - ));
    "*", (type_arithm, prim2_int ( * ));
    "/", (type_arithm, prim2_int ( / ));
  ]

let typing_env =
 [ "int", 0;
   "bool", 0; ],
  List.map
    (fun (id,(ty,_)) -> id, ty)
    primitives

let eval_env = List.map (fun (id, (_,f)) -> id, f) primitives
