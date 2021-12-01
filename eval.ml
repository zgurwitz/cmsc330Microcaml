open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable lookup " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable update " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Value a -> a 
  | ID x -> (lookup env x)
  | Not b -> (eval_Not env b) 
  | Binop (op, one, two) -> eval_Binop env (op, one, two)
  | If (guard, t, f) -> (match eval_expr env guard with 
      | Bool b -> if b = true then eval_expr env t else eval_expr env f
      | _ -> raise (TypeError "Expected type boolean as guard") )
  | Let (x,false,init,body) -> eval_expr (extend env x (eval_expr env init)) body 
  | Let (x,true,init,body) ->
      let newEnv = extend_tmp env x in
      update newEnv x (eval_expr (newEnv) init);
      eval_expr newEnv body
  | FunctionCall (c,d) -> 
      (match eval_expr env c with 
       |Closure (en, x, body) -> 
           let newEnv  = extend en x (eval_expr env d) in 
           eval_expr newEnv body
       |_ -> raise (TypeError "Expected type Closure")
      )
  | Fun (c, d) -> Closure (env, c, d)
                    
and eval_Not env e = 
  match e with
  | Value (Bool b) -> Bool (b = false) 
  | ID a -> (match (lookup env a) with |Bool a -> Bool (a = false) 
                                       |_ -> raise (TypeError "Expected type boolean")) 
  | ex -> (match (eval_expr env ex) with |Bool a -> Bool (a = false)
                                         |_ -> raise (TypeError "Expected type boolean")) 
    
          
and eval_Binop env e = 
  match e with 
  |(Add, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Int (first+second)
           |_ -> raise (TypeError "Add expected type int")) 
       |_ -> raise (TypeError "Add expected type int")) 
  |(Mult, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Int (first*second)
           |_ -> raise (TypeError "Mult expected type int")) 
       |_ -> raise (TypeError "Mult expected type int")) 
  |(Sub, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Int (first-second)
           |_ -> raise (TypeError "Sub expected type int")) 
       |_ -> raise (TypeError "Sub expected type int")) 
  |(Div, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int 0 -> raise (DivByZeroError)
           |Int second -> Int (first/second)
           |_ -> raise (TypeError "Div expected type int")) 
       |_ -> raise (TypeError "Div expected type int"))
            
  |(Greater, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Bool (first>second)
           |_ -> raise (TypeError "Greater expected type int")) 
       |_ -> raise (TypeError "Greater expected type int")) 
  |(Less, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Bool (first<second)
           |_ -> raise (TypeError "Less expected type int")) 
       |_ -> raise (TypeError "Less expected type int")) 
  |(GreaterEqual, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Bool (first>=second)
           |_ -> raise (TypeError "GreaterEqual expected type int")) 
       |_ -> raise (TypeError "GreaterEqual expected type int")) 
  |(LessEqual, b, c) -> 
      (match (eval_expr env b) with 
       |Int first -> (match (eval_expr env c) with 
           |Int second -> Bool (first<=second)
           |_ -> raise (TypeError "LessEqual expected type int")) 
       |_ -> raise (TypeError "LessEqual expected type int")) 
  |(Concat, b, c) -> 
      (match (eval_expr env b) with 
       |String first -> (match (eval_expr env c) with 
           |String second -> String (first^second)
           |_ -> raise (TypeError "Concat expected type string")) 
       |_ -> raise (TypeError "Concat expected type string")) 
      
  |(Equal,ex1,ex2) -> (match (eval_expr env ex1, eval_expr env ex2) with
      |(Int a, Int b) -> Bool (a = b)
      |(Bool a, Bool b) -> Bool (a = b)
      |(String a, String b) ->  Bool (a = b)
      |_ -> raise (TypeError "Cannot compare types")
    )
  |(NotEqual,ex1,ex2) -> (match (eval_expr env ex1, eval_expr env ex2) with
      |(Int a, Int b) -> Bool (a <> b)
      |(Bool a, Bool b) -> Bool (a <> b)
      |(String a, String b) ->  Bool (a <> b)
      |_ -> raise (TypeError "Cannot compare types")
    ) 
  |(Or,ex1,ex2) -> (match (eval_expr env ex1, eval_expr env ex2) with
      |(Bool a, Bool b) -> Bool (a || b)
      |_ ->  raise (TypeError "Expected type boolean")
    )
    
  |(And,ex1,ex2) -> (match (eval_expr env ex1, eval_expr env ex2) with
      |(Bool a, Bool b) -> Bool (a && b)
      |_ ->  raise (TypeError "Expected type boolean")
    )
          
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | NoOp -> (env, None)  
  | Expr e -> (env, Some (eval_expr env e))
  | Def (x, e) -> let envNew = extend_tmp env x in
      let value = (eval_expr envNew e) in
      update envNew x value;
      (envNew, Some value)