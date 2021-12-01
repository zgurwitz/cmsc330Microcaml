open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None


(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let (remaining_toks, expr) = parse_Expr toks in
    (*if remaining_toks <> [] then raise (InvalidInputException "still tokens left")*)
    (remaining_toks, expr)
and parse_Expr toks = 
  match (lookahead toks) with 
    | Some Tok_Let -> parse_LetExpr toks
    | Some Tok_If -> parse_IfExpr toks
    | Some Tok_Fun -> parse_FunctionExpr toks
    | Some Tok_Not | Some Tok_LParen -> parse_OrExpr toks 
    | Some Tok_Int a -> parse_OrExpr toks 
    | Some Tok_Bool a -> parse_OrExpr toks 
    | Some Tok_String a -> parse_OrExpr toks 
    | Some Tok_ID a -> parse_OrExpr toks 
    |_ -> raise (InvalidInputException (Printf.sprintf "%s is toks in parse_Expr" 
      (string_of_list string_of_token toks)))
and parse_LetExpr toks = 
    let toks2 = match_token toks Tok_Let in
       let (toks3, expr_after_Recursion) = parse_Recursion toks2 in
        let iD = match Option.get(lookahead toks3) with 
          | Tok_ID a -> a
          | _ -> failwith( "parse_letExpr")
        in
          let toks4 = match_token toks3 (Tok_ID iD) in
            let toks5 = match_token toks4 Tok_Equal in
              let (toks6, expr_after_Expr1) = parse_Expr toks5 in
                let toks7 = match_token toks6 Tok_In in
                  let (toks8, expr_after_Expr2) = parse_Expr toks7 in
                    (toks8, Let (iD, expr_after_Recursion, expr_after_Expr1, expr_after_Expr2))
and parse_Recursion toks = 
    match lookahead toks with
      | Some Tok_Rec -> (match_token toks Tok_Rec, true)
      | _ -> (toks, false)
and parse_FunctionExpr toks =
    let toks2 = match_token toks Tok_Fun in
      let iD = match get(lookahead toks2) with 
          | Tok_ID a -> a
          | _ -> failwith( "parse_FunctionExpr")
      in
        let toks3 = match_token toks2 (Tok_ID iD) in
          let toks4 = match_token toks3 Tok_Arrow in
            let (toks5, expr_after_Expr) = parse_Expr toks4 in
              (toks5, Fun (iD, expr_after_Expr))
and parse_IfExpr toks =
    let toks2 = match_token toks Tok_If in 
      let (toks3, expr_after_Expr1) = parse_Expr toks2 in
        let toks4 = match_token toks3 Tok_Then in 
          let (toks5, expr_after_Expr2) = parse_Expr toks4 in
            let toks6 = match_token toks5 Tok_Else in 
              let (toks7, expr_after_Expr3) = parse_Expr toks6 in
                (toks7, If(expr_after_Expr1, expr_after_Expr2, expr_after_Expr3))
and parse_OrExpr toks = 
    let (toks2, expr_after_And) = parse_AndExpr toks in
      match (lookahead toks2) with
      | Some Tok_Or -> 
        let (toks3, expr_after_OrSub) = parse_OrSub toks2 in
          (toks3, Binop(Or, expr_after_And, expr_after_OrSub))
      | _ -> 
        (toks2, expr_after_And)
      
and parse_OrSub toks = 
    match (lookahead toks) with
    | Some Tok_Or -> 
      let toks2 = match_token toks Tok_Or in
        let (toks3, expr_after_OrExpr) = parse_OrExpr toks2 in
          (toks3, expr_after_OrExpr)
    | _ -> failwith( "OrSub")
and parse_AndExpr toks = 
    
    let (toks2, expr_after_Equality) = parse_EqualityExpr toks in
    match (lookahead toks2) with
      
      | Some Tok_And -> 
        let (toks3, expr_after_AndSub) = parse_AndSub toks2 in
         (toks3, Binop (And, expr_after_Equality, expr_after_AndSub))
      | _ ->
        (toks2, expr_after_Equality)
      
and parse_AndSub toks = 
    match (lookahead toks) with
    | Some Tok_And -> 
      let toks2 = match_token toks Tok_And in
        let (toks3, expr_after_AndExpr) = parse_AndExpr toks2 in
          (toks3, expr_after_AndExpr) 
    | _ -> failwith( "AndSub")
and parse_EqualityExpr toks = 
  let (toks2, expr_after_RelEx) = parse_RelationalExpr toks in
    match (lookahead toks2) with 
    | Some Tok_Equal ->
      let (toks3, expr_after_EqSub) = parse_EqualitySub toks2 in
        (toks3, Binop (Equal, expr_after_RelEx, expr_after_EqSub))
    | Some Tok_NotEqual -> 
      let (toks3, expr_after_EqSub) = parse_EqualitySub toks2 in
        (toks3, Binop (NotEqual, expr_after_RelEx, expr_after_EqSub))
    | _ -> (toks2, expr_after_RelEx)
and parse_EqualitySub toks = 
  match (lookahead toks) with
    | Some Tok_Equal  -> 
      let toks2 = match_token toks Tok_Equal in
          let (toks3, expr_after_EqEx) = parse_EqualityExpr toks2 in
            (toks3, expr_after_EqEx)
    | Some Tok_NotEqual -> 
      let toks2 = match_token toks Tok_NotEqual in
          let (toks3, expr_after_EqEx) = parse_EqualityExpr toks2 in
            (toks3, expr_after_EqEx)  
    | _ -> failwith( "EqualitySub")
and parse_EqualityOp toks = 
  match (lookahead toks) with
    | Some Tok_Equal -> 
      let toks2 = match_token toks Tok_Equal in
        (toks2, Equal)
    | Some Tok_NotEqual -> 
      let toks2 = match_token toks Tok_NotEqual in
        (toks2, NotEqual)
    | _ -> failwith( "EqualityOp")
and parse_RelationalExpr toks = 
  let (toks2, expr_after_AddEx) = parse_AdditiveExpr toks in
    match (lookahead toks2) with
      |  Some Tok_Greater ->
        let toks3 = match_token toks2 Tok_Greater in
          let (toks4, expr_after_RelEx) = parse_RelationalExpr toks3
            in (toks4, Binop(Greater, expr_after_AddEx, expr_after_RelEx)) 
      |  Some Tok_GreaterEqual ->
        let toks3 = match_token toks2 Tok_GreaterEqual in
          let (toks4, expr_after_RelEx) = parse_RelationalExpr toks3
            in (toks4, Binop(GreaterEqual, expr_after_AddEx, expr_after_RelEx)) 
      |  Some Tok_Less -> 
        let toks3 = match_token toks2 Tok_Less in
          let (toks4, expr_after_RelEx) = parse_RelationalExpr toks3
            in (toks4, Binop(Less, expr_after_AddEx, expr_after_RelEx)) 
      |  Some Tok_LessEqual -> 
        let toks3 = match_token toks2 Tok_LessEqual in
          let (toks4, expr_after_RelEx) = parse_RelationalExpr toks3
            in (toks4, Binop(LessEqual, expr_after_AddEx, expr_after_RelEx)) 
      | _ -> (toks2, expr_after_AddEx)
and parse_AdditiveExpr toks = 
  let (toks2, expr_after_MultEx) = parse_MultExpr toks in
    match (lookahead toks2) with
      | Some Tok_Add -> 
        let toks3 = match_token toks2 Tok_Add in
          let (toks4, expr_after_AddEx) = parse_AdditiveExpr toks3 in
            (toks4, Binop(Add, expr_after_MultEx, expr_after_AddEx))
      | Some Tok_Sub -> 
        let toks3 = match_token toks2 Tok_Sub in
          let (toks4, expr_after_AddEx) = parse_AdditiveExpr toks3 in
            (toks4, Binop(Sub, expr_after_MultEx, expr_after_AddEx))
      | _ -> (toks2, expr_after_MultEx)
and parse_MultExpr toks = 
  let (toks2, expr_after_cat) = parse_ConcatExpr toks in
    match (lookahead toks2) with
      | Some Tok_Mult ->
        let toks3 = match_token toks2 Tok_Mult in
          let (toks4, expr_after_MultExpr) = parse_MultExpr toks3 in
            (toks4, Binop(Mult, expr_after_cat, expr_after_MultExpr))
      | Some Tok_Div ->
        let toks3 = match_token toks2 Tok_Div in
          let (toks4, expr_after_MultExpr) = parse_MultExpr toks3 in
            (toks4, Binop(Div, expr_after_cat, expr_after_MultExpr))
      | _ -> (toks2, expr_after_cat)
and parse_ConcatExpr toks = 
  let (toks2, expr_after_Unary) = parse_UnaryExpr toks in
    match (lookahead toks2) with
      | Some Tok_Concat ->
        let toks3 = match_token toks2 Tok_Concat in
          let (toks4, expr_after_cat) = parse_ConcatExpr toks3 in
            (toks4, Binop(Concat, expr_after_Unary, expr_after_cat))
      | _ -> (toks2, expr_after_Unary)
and parse_UnaryExpr toks = 
  match (lookahead toks) with
    | Some Tok_Not -> 
      let toks2 = match_token toks Tok_Not in
        let (toks3, expr_after_Unary) = parse_UnaryExpr toks2 in
          (toks3, Not(expr_after_Unary))
    | _ -> parse_FCallExpr toks
and parse_FCallExpr toks = 
  let (toks2, expr_after_Primary) = parse_PrimaryExpr toks in
    match (lookahead toks2) with
    | Some Tok_Int a ->
      let (toks3, expr_after_Primary2) = parse_PrimaryExpr toks2 in
        (toks3, FunctionCall(expr_after_Primary, expr_after_Primary2))
    | Some Tok_Bool a ->
      let (toks3, expr_after_Primary2) = parse_PrimaryExpr toks2 in
        (toks3, FunctionCall(expr_after_Primary, expr_after_Primary2))
    | Some Tok_String a ->
      let (toks3, expr_after_Primary2) = parse_PrimaryExpr toks2 in
        (toks3, FunctionCall(expr_after_Primary, expr_after_Primary2))
    | Some Tok_ID a ->
      let (toks3, expr_after_Primary2) = parse_PrimaryExpr toks2 in
        (toks3, FunctionCall(expr_after_Primary, expr_after_Primary2))
    | Some Tok_LParen ->
      let (toks3, expr_after_Primary2) = parse_PrimaryExpr toks2 in
        (toks3, FunctionCall(expr_after_Primary, expr_after_Primary2))
    | _ ->  (toks2, expr_after_Primary)
and parse_PrimaryExpr toks = 
  match (lookahead toks) with
    | Some Tok_Int a -> let toks2 = match_token toks (Tok_Int a) in
      (toks2, Value (Int a))
    | Some Tok_Bool b -> let toks2 = match_token  toks (Tok_Bool b) in
      (toks2, Value (Bool b))
    | Some Tok_String c -> let toks2 = match_token  toks (Tok_String c) in
      (toks2, Value (String c))
    | Some Tok_ID d -> let toks2 = match_token toks (Tok_ID d) in
      (toks2, ID d)
    | Some Tok_LParen -> let toks2 = match_token toks Tok_LParen in
      let (toks3, expr_after_Expr) = parse_Expr toks2 in
        let toks4 = match_token  toks3 Tok_RParen in
          (toks4, expr_after_Expr)
    | _ -> raise (InvalidInputException (Printf.sprintf "%s is toks in parse_PrimaryExpr" 
      (string_of_list string_of_token toks)))
  
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match (lookahead toks) with
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | Some Tok_Def -> parse_DefMutop toks
  | _ -> parse_ExprMutop toks
and parse_DefMutop toks = 
  let toks2 = match_token toks Tok_Def in
    let (toks3, iD) = 
      match (lookahead toks2) with
      | Some Tok_ID a -> (match_token toks2 (Tok_ID a), a)
      | _ ->  raise (InvalidInputException (Printf.sprintf "%s is toks in parse_DefMutop" 
      (string_of_list string_of_token toks)))
    in 
      let toks4 = match_token toks3 Tok_Equal in
        let (toks5, expr_after_Expr) = parse_expr toks4 in
          let toks6 = match_token toks5 Tok_DoubleSemi in
            (toks6, Def(iD , expr_after_Expr)) 
and parse_ExprMutop toks = 
  let (toks2, expr_after_Expr) = parse_expr toks in
    (match_token toks2 Tok_DoubleSemi, Expr(expr_after_Expr))