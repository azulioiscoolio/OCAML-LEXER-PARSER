open TokenTypes
open Str 
open String
(* PASTE YOUR LEXER FROM P4A HERE *)

let rec tokenize input = let lparen = Str.regexp "(" in let rparen = Str.regexp ")" in let equal = Str.regexp "=" in let notEqual = Str.regexp "<>"
  in let gt = Str.regexp ">" in let lt = Str.regexp "<" in let gte = Str.regexp ">=" in let lte = Str.regexp "<=" in let orRegex = Str.regexp "||" in 
  let andRegex = Str.regexp "&&" in let notRegex = Str.regexp "not" in let ifRegex = Str.regexp "if" in let thenRegex = Str.regexp "then" in 
  let elseRegex = Str.regexp "else" in let defRegex = Str.regexp "def" in 
  let add = Str.regexp "+" in let sub = Str.regexp "-" in let mult = Str.regexp "*" in let div = Str.regexp "/" in let concat = Str.regexp "\\^" in 
  let letRegex = Str.regexp "let" in let inRegex = Str.regexp "in" in let recRegex = Str.regexp "rec" in let funRegex = Str.regexp "fun" in 
  let arrow = Str.regexp "->" in let doubleSemi = Str.regexp ";;" in let intRegex = Str.regexp "(\\([-][0-9]+\\))\\|[0-9]+" in let boolRegex = Str.regexp "true\\|false" in 
  let stringRegex = Str.regexp "\"\\([^\"]*\\)\"" in let ws = Str.regexp "[ \t\n]" in let idStuff = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in 
  let length = String.length input  in

  let rec tokenH pos = if pos >= length then [] 

  else if Str.string_match intRegex input pos then
    let newPos = Str.match_end() in let str = Str.matched_string input in
    if Str.string_match (Str.regexp "(\\([-][0-9]+\\))") input (pos) then let x = (Str.matched_group 1 input) in Tok_Int(int_of_string(x))::(tokenH (newPos)) 
    else Tok_Int(int_of_string (str))::(tokenH (newPos))

  else if Str.string_match stringRegex input pos then 
    let newPos = Str.match_end() in let str = matched_group 1 input in 
    Tok_String(str)::(tokenH (newPos))

  else if Str.string_match boolRegex input pos then 
    let newPos = Str.match_end() in let x = Str.matched_string input in 
    Tok_Bool(bool_of_string (x))::(tokenH (newPos))

  else if Str.string_match lparen input pos then 
    Tok_LParen::(tokenH (pos + 1))

  else if Str.string_match rparen input pos then 
    Tok_RParen::(tokenH (pos + 1))

  else if Str.string_match equal input pos then 
    Tok_Equal::(tokenH (pos + 1))

  else if Str.string_match notEqual input pos then 
    Tok_NotEqual::(tokenH (pos + 2))

  else if Str.string_match gt input pos then
    Tok_Greater::(tokenH (pos + 1))
        
  else if Str.string_match lt input pos then 
    Tok_Less::(tokenH (pos + 1))

  else if Str.string_match gte input pos then 
    Tok_GreaterEqual::(tokenH (pos + 2))

  else if Str.string_match lte input pos then 
    Tok_LessEqual::(tokenH (pos + 2))

  else if Str.string_match orRegex input pos then 
    Tok_Or::(tokenH (pos + 2))

  else if Str.string_match andRegex input pos then 
    Tok_And::(tokenH (pos + 2)) 

  else if Str.string_match notRegex input pos then 
    Tok_Not::(tokenH (pos + 3)) 
  
  else if Str.string_match add input pos then 
    Tok_Add::(tokenH (pos + 1))

  else if Str.string_match arrow input pos then 
    Tok_Arrow::(tokenH (pos + 2))

  else if Str.string_match sub input pos then 
    Tok_Sub::(tokenH (pos + 1))

  else if Str.string_match mult input pos then 
    Tok_Mult::(tokenH (pos + 1))

  else if Str.string_match div input pos then 
    Tok_Div::(tokenH (pos + 1))

  else if Str.string_match concat input pos then 
    Tok_Concat::(tokenH (pos + 1))

  else if Str.string_match doubleSemi input pos then 
    Tok_DoubleSemi::(tokenH (pos + 2))

  else if Str.string_match ifRegex input pos then let if2 = Str.regexp "if[a-zA-Z0-9]+|[a-zA-Z0-9]+if|[a-zA-Z0-9]+if[a-zA-Z0-9]+" in 
    if Str.string_match if2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_If::(tokenH (pos + 2))

  else if Str.string_match thenRegex input pos then let then2 = Str.regexp "then[a-zA-Z0-9]+|[a-zA-Z0-9]+then|[a-zA-Z0-9]+then[a-zA-Z0-9]+" in 
    if Str.string_match then2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Then::(tokenH (pos + 4))

  else if Str.string_match elseRegex input pos then let else2 = Str.regexp "else[a-zA-Z0-9]+|[a-zA-Z0-9]+else|[a-zA-Z0-9]+else[a-zA-Z0-9]+" in 
    if Str.string_match else2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Else::(tokenH (pos + 4))

  else if Str.string_match letRegex input pos then let let2 = Str.regexp "let[a-zA-Z0-9]+|[a-zA-Z0-9]+let|[a-zA-Z0-9]+let[a-zA-Z0-9]+" in 
    if Str.string_match let2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Let::(tokenH (pos + 3))

  else if Str.string_match defRegex input pos then let def2 = Str.regexp "def[a-zA-Z0-9]+|[a-zA-Z0-9]+def|[a-zA-Z0-9]+def[a-zA-Z0-9]+" in 
    if Str.string_match def2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Def::(tokenH (pos + 3))

  else if Str.string_match inRegex input pos then let in2 = Str.regexp "in[a-zA-Z0-9]+|[a-zA-Z0-9]+in|[a-zA-Z0-9]+in[a-zA-Z0-9]+" in 
    if Str.string_match in2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_In::(tokenH (pos + 2))

  else if Str.string_match recRegex input pos then let rec2 = Str.regexp "rec[a-zA-Z0-9]+|[a-zA-Z0-9]+rec|[a-zA-Z0-9]+rec[a-zA-Z0-9]+" in 
    if Str.string_match rec2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Rec::(tokenH (pos + 3))

  else if Str.string_match funRegex input pos then let fun2 = Str.regexp "fun[a-zA-Z0-9]+|[a-zA-Z0-9]+fun|[a-zA-Z0-9]+fun[a-zA-Z0-9]+" in 
    if Str.string_match fun2 input pos then
      Tok_ID(Str.matched_string input)::(tokenH (Str.match_end()))
    else Tok_Fun::(tokenH (pos + 3))

  else if Str.string_match idStuff input pos then 
    let newPos = Str.match_end() in let x = (Str.matched_string input) in 
    Tok_ID(x)::(tokenH (newPos))

  else if Str.string_match ws input pos then 
    let newPos = Str.match_end() in tokenH newPos

  else
    raise (InvalidInputException("fail"))
  in tokenH 0;;