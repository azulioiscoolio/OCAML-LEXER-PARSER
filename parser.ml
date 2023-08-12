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

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = match lookahead toks with 
| Some Tok_Let -> parse_let toks
| Some Tok_If -> parse_if toks
| Some Tok_Fun -> parse_fun toks
| _-> parse_or toks

and parse_let toksH = let t = match_token toksH Tok_Let in 
  match lookahead t with 
  |Some Tok_Rec -> (let t = match_token t Tok_Rec in match lookahead t with 
    |Some Tok_ID(tokID) -> (let t = match_token t (Tok_ID(tokID)) in match lookahead t with
      |Some Tok_Equal->(let t = match_token t Tok_Equal in let (lst, ast) = parse_expr t in match lookahead lst with
        |Some Tok_In -> let t = match_token lst Tok_In in let (lst2, ast2) = parse_expr t in (lst2, Let(tokID, true, ast, ast2))
        | _ -> raise (InvalidInputException("let fail3")))    
      | _ -> raise (InvalidInputException("let fail2")))    
    | _ -> raise (InvalidInputException("let fail1")))    

  |_-> (match lookahead t with 
    |Some Tok_ID(tokID) -> (let t = match_token t (Tok_ID(tokID)) in match lookahead t with
      |Some Tok_Equal->(let t = match_token t Tok_Equal in let (lst, ast) = parse_expr t in match lookahead lst with
        |Some Tok_In -> let t = match_token lst Tok_In in let (lst2, ast2) = parse_expr t in (lst2, Let(tokID, false, ast, ast2))
        | _ -> raise (InvalidInputException("no rec fail")))    
      | _ -> raise (InvalidInputException("no recfail")))    
    | _ -> raise (InvalidInputException("no recfail")))   

and parse_fun toksH = let t = match_token toksH Tok_Fun in match lookahead t with 
  |Some Tok_ID(tokID) -> (let t = match_token t (Tok_ID(tokID)) in match lookahead t with 
      |Some Tok_Arrow -> let t = match_token t Tok_Arrow in let (lst, ast) = parse_expr t in (lst, Fun(tokID, ast))
      | _ -> raise (InvalidInputException("fun fail")))
  | _ -> raise (InvalidInputException("fun fail"))

and parse_if toksH = let t = match_token toksH Tok_If in let (lst, ast) = parse_expr t in match lookahead lst with 
 |Some Tok_Then -> (let t = match_token lst Tok_Then in let (lst2, ast2) = parse_expr t in match lookahead lst2 with
  |Some Tok_Else -> let t = match_token lst2 Tok_Else in let (lst3, ast3) = parse_expr t in (lst3,If(ast, ast2, ast3))
  | _ -> raise (InvalidInputException("iffail"))) 
 | _ -> raise (InvalidInputException("iffail")) 

and parse_or toksH = let (lst, ast) = parse_and toksH in match lookahead lst with 
  |Some Tok_Or -> let t = match_token lst Tok_Or in let (lst2, ast2) = parse_or t in (lst2,Binop(Or, ast, ast2))
  | _ -> (lst, ast)

and parse_and toksH = let (lst, ast) = parse_equality toksH in match lookahead lst with 
|Some Tok_And -> let t = match_token lst Tok_And in let (lst2, ast2) = parse_and t in (lst2,Binop(And, ast, ast2))
| _ -> (lst, ast)

and parse_equality toksH = let (lst, ast) = parse_relational toksH in match lookahead lst with 
  |Some Tok_Equal -> let t = match_token lst Tok_Equal in let (lst2,ast2) = parse_equality t in (lst2, Binop(Equal, ast, ast2))
  |Some Tok_NotEqual -> let t = match_token lst Tok_NotEqual in let (lst2, ast2) = parse_equality t in (lst2, Binop(NotEqual, ast, ast2))      
  | _-> (lst, ast)

and parse_relational toksH = let (lst, ast) = parse_additive toksH in match lookahead lst with 
 |Some Tok_Less -> let t = match_token lst Tok_Less in let (lst2, ast2) = parse_relational t in (lst2, Binop(Less, ast, ast2))
 |Some Tok_Greater -> let t = match_token lst Tok_Greater in let (lst2, ast2) = parse_relational t in (lst2, Binop(Greater, ast, ast2))
 |Some Tok_LessEqual -> let t = match_token lst Tok_LessEqual in let (lst2, ast2) = parse_relational t in (lst2, Binop(LessEqual, ast, ast2))
 |Some Tok_GreaterEqual -> let t = match_token lst Tok_GreaterEqual in let (lst2, ast2) = parse_relational t in (lst2, Binop(GreaterEqual, ast, ast2))
 | _ -> (lst, ast)

and parse_additive toksH = let(lst, ast) = parse_multi toksH in match lookahead lst with 
 |Some Tok_Add -> let t = match_token lst Tok_Add in let (lst2, ast2) = parse_additive t in (lst2, Binop(Add, ast, ast2))
 |Some Tok_Sub -> let t = match_token lst Tok_Sub in let (lst2, ast2) = parse_additive t in (lst2, Binop(Sub, ast, ast2))
 | _ -> (lst, ast)

and parse_multi toksH = let(lst, ast) = parse_concat toksH in match lookahead lst with 
|Some Tok_Mult -> let t = match_token lst Tok_Mult in let (lst2, ast2) = parse_multi t in (lst2, Binop(Mult, ast, ast2))
|Some Tok_Div -> let t = match_token lst Tok_Div in let (lst2, ast2) = parse_multi t in (lst2, Binop(Div, ast, ast2))
| _-> (lst, ast) 

and parse_concat toksH = let(lst, ast) = parse_unary toksH in match lookahead lst with 
 |Some Tok_Concat -> let t = match_token lst Tok_Concat in let(lst2, ast2) = parse_concat t in (lst2, Binop(Concat, ast, ast2)) 
 | _-> (lst, ast)

and parse_unary toksH = match lookahead toksH with 
  |Some Tok_Not -> let t = match_token toksH Tok_Not in let (lst, ast) = parse_unary t in (lst,Not(ast))
  | _ -> let (lst, ast) = parse_fcall toksH in (lst,ast) 

and parse_fcall toksH = let (lst, ast) = parse_primary toksH in match lookahead lst with 
|Some Tok_Int(i)-> let (lst2, ast2) = parse_primary lst in (lst2,FunctionCall(ast,ast2))
|Some Tok_Bool(b) ->  let (lst2, ast2) = parse_primary lst in (lst2, FunctionCall(ast,ast2))
|Some Tok_String(str) ->  let (lst2, ast2) = parse_primary lst in (lst2, FunctionCall(ast,ast2))
|Some Tok_ID(id) ->  let (lst2, ast2) = parse_primary lst in (lst2,FunctionCall(ast,ast2))
|Some Tok_LParen ->  let (lst2, ast2) = parse_primary lst in (lst2,FunctionCall(ast,ast2)) 
|_ -> (lst, ast)

and parse_primary toksH = match lookahead toksH with 
|Some Tok_Int(i) -> (match_token toksH (Tok_Int(i)), Value(Int i))
|Some Tok_Bool(b) -> (match_token toksH (Tok_Bool(b)), Value(Bool b))
|Some Tok_String(str) -> (match_token toksH (Tok_String(str)), Value(String str))
|Some Tok_ID(id) -> (match_token toksH (Tok_ID(id)), ID(id))
|Some Tok_LParen -> (let (lst, ast) = parse_expr (match_token toksH Tok_LParen) in match lookahead lst with 
  |Some Tok_RParen -> let t = match_token lst Tok_RParen in (t,ast)
  |_ -> raise (InvalidInputException("prim fail1")))
| _-> raise (InvalidInputException("prim fail2"))


let rec parse_mutop toks =  match lookahead toks with 
 |Some Tok_Def -> (let t = match_token toks Tok_Def in match lookahead t with 
  |Some Tok_ID(id) -> (let t = match_token t (Tok_ID(id)) in match lookahead t with 
   |Some Tok_Equal ->(let t = match_token t Tok_Equal in let (lst, ast) = parse_expr t in match lookahead lst with
    |Some Tok_DoubleSemi -> let t = match_token lst Tok_DoubleSemi in (t, Def(id, ast))
    | _ -> raise (InvalidInputException("fail 1 mutop"))) 
   | _ -> raise (InvalidInputException("fail 2 mutop")))
  | _ -> raise (InvalidInputException("fail 3 mutop")))
 |Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
 | _-> let (lst, ast) = parse_expr toks in match lookahead lst with
  |Some Tok_DoubleSemi -> let t = match_token lst Tok_DoubleSemi in (t, Expr(ast))
  |_-> raise (InvalidInputException("fail expr mutop"))  