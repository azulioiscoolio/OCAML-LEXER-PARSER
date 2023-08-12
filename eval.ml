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

(*let extend env x v = (x,v)::env*)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(*let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x*)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(*let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)*)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
|Value v -> v
|ID x -> ref_lookup env x 
|Not expr ->(let t = eval_expr env expr in match t with 
  |Bool b -> Bool (not b)
  |_-> raise (TypeError("Expected type bool")))

|If(expr1,expr2,expr3) -> (let a = eval_expr env expr1 in match a with 
  |Bool b -> (match b with 
    |true-> eval_expr env expr2 
    |false-> eval_expr env expr3)
  |_-> raise (TypeError("Expected type bool if")))

|Let(var, b, expr, expr2) -> (match b with 
  |false ->let a = eval_expr env expr in let newenv = ref_extend env var a in eval_expr newenv expr2
  |true -> let env = ref_extend_tmp env var in let a = eval_expr env expr in 
    ref_update env var a;
    eval_expr env expr2)

|Binop(op, expr1, expr2) ->(match op with 
  |Add->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Int (i + j)
    |_ -> raise (TypeError("Expected type Int add")))
  |Sub->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Int (i - j)
    |_-> raise (TypeError("Expected type Int sub")))
  |Mult->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Int (i * j)
    |_-> raise (TypeError("Expected type Int mult")))
  |Div->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> if j = 0 then raise (DivByZeroError) else Int (i / j)
    |_-> raise(TypeError("Expected type Int div")))
  |Greater->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (i > j)
    |_-> raise (TypeError("Expected type Int greater")))
  |Less->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (i < j)
    |_-> raise (TypeError("Expected type Int less")))
  |GreaterEqual->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (i >= j)
    |_-> raise (TypeError("Expected type Int greater eql")))
  |LessEqual->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (i <= j)
    |_-> raise (TypeError("Expected type Int less equal")))
  |Concat->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |String i, String j -> String (i ^ j)
    |_-> raise (TypeError("Expected type String")))
  |Equal->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (i = j)
    |_-> raise (TypeError("Expected type Int equal")))
  |NotEqual->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Int i, Int j -> Bool (not(i = j))
    |_-> raise (TypeError("Expected type Int not equal")))
  |Or->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Bool i, Bool j -> Bool (i || j)
    |_-> raise (TypeError("Expected type bool or")))
  |And->(let a = eval_expr env expr1 in let b = eval_expr env expr2 in match a, b with 
    |Bool i, Bool j -> Bool (i && j)
    |_-> raise (TypeError("Expected type bool and"))))

|Fun(var, expr)-> (Closure (env, var, expr))
|FunctionCall(expr, expr2) -> (let a = eval_expr env expr in match a with 
  |Closure(fun_env, var, ex) -> let b = eval_expr env expr2 in eval_expr (ref_extend fun_env var b) ex 
  |_-> raise (TypeError("function call")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
  |NoOp -> (env, None)
  |Def(var, expr) -> let env2 = ref_extend_tmp env var in let a = eval_expr env2 expr in ref_update env2 var a;
    (env2, Some a)
  |Expr(expr) -> (env, Some (eval_expr env expr))