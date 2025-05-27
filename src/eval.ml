open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

let rec eval_expr env e = match e with
| Value(val1) -> (match val1 with
  | Int(int) -> Int(int)
  | Bool(bool) -> Bool(bool)
  | String(string) -> String(string)
  | Closure(env', var, exp) -> Closure(env', var, exp))
| ID(var) -> ref_lookup env var
| Not(exp) -> (let exp_result = eval_expr env exp in
  (match exp_result with
  | Bool(bool) -> if bool = true then Bool(false) else
                  if bool = false then Bool(true) else
                  raise (TypeError "Expression after not is not a boolean")
  | _ -> raise (TypeError "Expected type booelan")))
| Binop(op, exp1, exp2) -> (match op with
  | Add -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 + int2 in Int(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Sub -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 - int2 in Int(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Mult -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 * int2 in Int(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Div -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> if int2 = 0 then raise (DivByZeroError) else let result = int1 / int2 in Int(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Greater -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 > int2 in Bool(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Less -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 < int2 in Bool(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | GreaterEqual -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 >= int2 in Bool(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | LessEqual -> (match eval_expr env exp1 with
          | Int(int1) -> (match eval_expr env exp2 with
                        | Int(int2) -> let result = int1 <= int2 in Bool(result)
                        | _ -> raise (TypeError "Integer type expected"))
          | _ -> raise (TypeError "Integer type expected"))
  | Concat -> (match eval_expr env exp1 with
              | String(string1) -> (match eval_expr env exp2 with
                                    | String(string2) -> let result = string1 ^ string2 in String(result)
                                    | _ -> raise (TypeError "String type expected"))
              | _ -> raise (TypeError "String type expected"))
  | Equal -> (match eval_expr env exp1 with
              | Int(int1) -> (match eval_expr env exp2 with
                              | Int(int2) -> let result = (int1 = int2) in Bool(result)
                              | _ -> raise (TypeError "Cannot compare mismatch types"))
              | Bool(bool1) -> (match eval_expr env exp2 with
                              | Bool(bool2) -> let result = (bool1 = bool2) in Bool(result)
                              | _ -> raise (TypeError "Cannot compare mismatch types"))
              | String(string1) -> (match eval_expr env exp2 with 
                                  | String(string2) -> let result = (string1 = string2) in Bool(result)
                                  | _ -> raise (TypeError "Cannot compare mismatch types"))
              | _ -> raise (TypeError "Cannot compare closure"))
  | NotEqual -> (match eval_expr env exp1 with
              | Int(int1) -> (match eval_expr env exp2 with
                              | Int(int2) -> let result = (int1 <> int2) in Bool(result)
                              | _ -> raise (TypeError "Cannot compare mismatch types"))
              | Bool(bool1) -> (match eval_expr env exp2 with
                              | Bool(bool2) -> let result = (bool1 <> bool2) in Bool(result)
                              | _ -> raise (TypeError "Cannot compare mismatch types"))
              | String(string1) -> (match eval_expr env exp2 with 
                                  | String(string2) -> let result = (string1 <> string2) in Bool(result)
                                  | _ -> raise (TypeError "Cannot compare mismatch types"))
              | _ -> raise (TypeError "Cannot compare closure"))
  | Or -> (match eval_expr env exp1 with
          | Bool(bool1) -> (match eval_expr env exp2 with
                          | Bool(bool2) -> let result = (bool1 || bool2) in Bool(result)
                          | _ -> raise (TypeError "Exepected type bool"))
          | _ -> raise (TypeError "Expected type bool"))
  | And -> (match eval_expr env exp1 with
          | Bool(bool1) -> (match eval_expr env exp2 with
                          | Bool(bool2) -> let result = (bool1 && bool2) in Bool(result)
                          | _ -> raise (TypeError "Exepected type bool"))
          | _ -> raise (TypeError "Expected type bool")))
| If(guard, t_branch, f_branch) -> (match eval_expr env guard with
  | Bool(bool) -> if bool then eval_expr env t_branch else eval_expr env f_branch
  | _ -> raise (TypeError "Expected guard to evaluate to type bool"))
| Let(var, is_rec, init, body) -> (if is_rec then 
                (let new_env = ref_extend_tmp env var in
                let init_result = eval_expr new_env init in
                let () = ref_update new_env var init_result in
                eval_expr new_env body)
        else
                (let init_result = eval_expr env init in let new_env = ref_extend env var init_result in
                eval_expr new_env body))
| Fun(var, exp') -> Closure(env, var, exp')
| FunctionCall(exp1, exp2) -> (let first_closure = eval_expr env exp1 in
        (match first_closure with
        | Closure(env', var, body) -> let val1 = eval_expr env exp2 in 
                let new_env = ref_extend env' var val1 in eval_expr new_env body
        | _ -> raise (TypeError "Not a function")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
    | Def(var, exp) -> let new_env = ref_extend_tmp env var in
                        let result = eval_expr new_env exp in
                        let () = ref_update new_env var result in
                        (new_env, Some(result))
    | Expr(exp) -> (env, Some((eval_expr env exp)))
    | NoOp -> (env, None)
