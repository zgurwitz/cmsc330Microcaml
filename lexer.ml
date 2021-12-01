open TokenTypes
open Str
open String
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

exception IllegalExpression of string * int 
let not_another_token input =
    (input <> "true" && input <> "false" && input <> "then" && input <> "else" 
    && input <> "not" && input <> "let" && input <> "def" && input <> "rec" 
    && input <> "fun" && input <> "in" && input <> "if")
let tokenize input = 
    let rec tok pos s = 
        if pos >= String.length s then
            []
        else
            if (Str.string_match (Str.regexp "[0-9]+\\|(-[0-9]+)") s pos) then
                let token = Str.matched_string s in
                    let num = if (String.length token > 1 && String.sub token 1 1 = "-") then int_of_string (String.sub token 1 ((String.length token) -2))
                    else int_of_string token in
                        (Tok_Int num)::(tok (pos + (String.length token)) s)
            else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") s pos && 
            not_another_token (Str.matched_string s)) then
                let token = Str.matched_string s in
                    (Tok_ID token)::(tok (pos+ (String.length token)) s)
            else if (Str.string_match (Str.regexp "then") s pos) then
                    (Tok_Then)::(tok (pos+4) s)
            else if (Str.string_match (Str.regexp "else") s pos) then
                    (Tok_Else)::(tok (pos+4) s)
            else if (Str.string_match (Str.regexp "not") s pos) then
                    (Tok_Not)::(tok (pos+3) s)  
            else if (Str.string_match (Str.regexp "let") s pos) then
                    (Tok_Let)::(tok (pos+3) s)
            else if (Str.string_match (Str.regexp "def") s pos) then
                    (Tok_Def)::(tok (pos+3) s)
            else if (Str.string_match (Str.regexp "rec") s pos) then
                    (Tok_Rec)::(tok (pos+3) s)
            else if (Str.string_match (Str.regexp "fun") s pos) then
                    (Tok_Fun)::(tok (pos+3) s)
            else if (Str.string_match (Str.regexp "-\\>") s pos) then
                    (Tok_Arrow)::(tok (pos+2) s)
            else if (Str.string_match (Str.regexp ";;") s pos) then
                    (Tok_DoubleSemi)::(tok (pos+2) s)  
            else if (Str.string_match (Str.regexp "in") s pos) then
                    (Tok_In)::(tok (pos+2) s)
            else if (Str.string_match (Str.regexp "if") s pos) then
                    (Tok_If)::(tok (pos+2) s) 
            else if (Str.string_match (Str.regexp "\\>=") s pos) then
                    (Tok_GreaterEqual)::(tok (pos+2) s)
            else if (Str.string_match (Str.regexp "\\<=") s pos) then
                    (Tok_LessEqual)::(tok (pos+2) s)
            else if (Str.string_match (Str.regexp "||") s pos) then
                    (Tok_Or)::(tok (pos+2) s)      
            else if (Str.string_match (Str.regexp "&&") s pos) then
                    (Tok_And)::(tok (pos+2) s)
            else if (Str.string_match (Str.regexp "\\<\\>") s pos) then
                    (Tok_NotEqual)::(tok (pos+2) s) 
            else if (Str.string_match (Str.regexp "\n\\|\r\\|\t\\| ") s pos) then
                    (tok (pos+1) s)  
            else if (Str.string_match (Str.regexp "[)]") s pos) then
                    (Tok_RParen)::(tok (pos+1) s)                          
            else if (Str.string_match (Str.regexp "[(]") s pos) then
                    (Tok_LParen)::(tok (pos+1) s) 
            else if (Str.string_match (Str.regexp "=") s pos) then 
                    (Tok_Equal)::(tok (pos+1) s) 
            else if (Str.string_match (Str.regexp "\\>") s pos) then
                    (Tok_Greater)::(tok (pos+1) s)
            else if (Str.string_match (Str.regexp "\\<") s pos) then
                    (Tok_Less)::(tok (pos+1) s) 
            else if (Str.string_match (Str.regexp "\\+") s pos) then
                    (Tok_Add)::(tok (pos+1) s)
            else if (Str.string_match (Str.regexp "-") s pos) then
                    (Tok_Sub)::(tok (pos+1) s)    
            else if (Str.string_match (Str.regexp "\\*") s pos) then
                    (Tok_Mult)::(tok (pos+1) s)
            else if (Str.string_match (Str.regexp "/") s pos) then
                    (Tok_Div)::(tok (pos+1) s)  
            else if (Str.string_match (Str.regexp "\\^") s pos) then
                    (Tok_Concat)::(tok (pos+1) s)
            else if (Str.string_match (Str.regexp "true\\|false") s pos) then
                let token = Str.matched_string s in
                    (Tok_Bool (if (token = "true") then true else false))::
                    (tok (pos+ (String.length token)) s) 
            else if (Str.string_match (Str.regexp "\"[^\"]*\"") s pos) then
                let token = sub (Str.matched_string s) 1 
                ((String.length (Str.matched_string s)) - 2)  in
                    (Tok_String token)::(tok (pos + (String.length token) + 2) s)
            else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") s pos) then
                let token = Str.matched_string s in
                    (Tok_ID token)::(tok (pos+ (String.length token)) s)
            else raise (IllegalExpression ("no match!", pos)) 
    in tok 0 input