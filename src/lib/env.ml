type name = string

type t = (name * Value.t) list

let lookup env n =
  try
    List.assoc n env
  with
    Not_found -> failwith ("lookup_env: " ^ n)

let update env (n,v) =
  let rec scan l = match l with
    | [] -> [n,v]
    | (n',v')::rest -> if n=n' then (n,v)::rest else (n',v') :: scan rest in
  scan env

let to_string env = Misc.string_of_list (fun (n,v) -> n ^ "=" ^ Value.to_string v) "," env
                      
