open TokenTypes

let rParenRegex = Str.regexp ")";;  
let lParenRegex = Str.regexp "(";; 
let equalRegex = Str.regexp "=";; 
let notEqualRegex = Str.regexp "<>";; 
let greaterEqualRegex = Str.regexp ">=";; 
let lessEqualRegex = Str.regexp "<=";; 
let greaterRegex = Str.regexp ">";; 
let lessRegex = Str.regexp "<";; 
let orRegex = Str.regexp "||";; 
let andRegex = Str.regexp "&&";; 
let notRegex = Str.regexp "not";; 
let ifRegex = Str.regexp "if";; 
let thenRegex = Str.regexp "then";; 
let elseRegex = Str.regexp "else";; 
let addRegex = Str.regexp "+";; 
let subRegex = Str.regexp "-";; 
let multRegex = Str.regexp "*";; 
let divRegex = Str.regexp "\\/";; 
let concatRegex = Str.regexp {|\^|};; 
let letRegex = Str.regexp "let";; 
let recRegex = Str.regexp "rec";; 
let inRegex = Str.regexp "in";; 
let defRegex = Str.regexp "def";; 
let funRegex = Str.regexp "fun";; 
let arrowRegex = Str.regexp "->";; 
let intofintRegex = Str.regexp {|\([0-9]+\)\|(-[0-9]+)|};; 
let boolofboolRegex = Str.regexp {|true\|false|};; 
let stringofstringRegex = Str.regexp {|"[^\"]*"|};; 
let idofstringRegex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let doubleSemiRegex = Str.regexp ";;";;
let whiteSpaceRegex = Str.regexp "[ \t\n]+";;

let rec trim_quotes input balanced = 
  if String.length input = 0 && balanced then input 
  else if balanced then 
    (if String.get input 0 = '"' then trim_quotes (String.sub input 1 (String.length input - 1)) false else input)
  else if String.length input = 0 then raise (InvalidInputException "Improper quotes")
  else if String.get input ((String.length input) - 1) = '"' then trim_quotes (String.sub input 0 (String.length input - 1)) true
  else input

let check_if_ID input snippet = 
  if Str.string_match idofstringRegex input 0 then 
    let matched = Str.matched_string input in
    if String.length matched > String.length snippet then matched else snippet
  else snippet

let rec tokenize_helper input acc = 
  let length = String.length input in
  if length = 0 then acc else
  if Str.string_match whiteSpaceRegex input 0 then let matched_len = String.length (Str.matched_string input) in
    tokenize_helper (Str.string_after input matched_len) acc else
  if Str.string_match doubleSemiRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_DoubleSemi]) else
  if Str.string_match arrowRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_Arrow]) else
  if Str.string_match notEqualRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_NotEqual]) else
  if Str.string_match greaterEqualRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_GreaterEqual]) else
  if Str.string_match lessEqualRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_LessEqual]) else
  if Str.string_match orRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_Or]) else
  if Str.string_match andRegex input 0 then tokenize_helper (Str.string_after input 2) (acc @ [Tok_And]) else
  if Str.string_match letRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "let" then tokenize_helper (Str.string_after input 3) (acc @ [Tok_Let]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match recRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "rec" then tokenize_helper (Str.string_after input 3) (acc @ [Tok_Rec]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match inRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "in" then tokenize_helper (Str.string_after input 2) (acc @ [Tok_In]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match defRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "def" then tokenize_helper (Str.string_after input 3) (acc @ [Tok_Def]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match funRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "fun" then tokenize_helper (Str.string_after input 3) (acc @ [Tok_Fun]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match ifRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "if" then tokenize_helper (Str.string_after input 2) (acc @ [Tok_If]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match thenRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "then" then tokenize_helper (Str.string_after input 4) (acc @ [Tok_Then]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match elseRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "else" then tokenize_helper (Str.string_after input 4) (acc @ [Tok_Else]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match notRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "not" then tokenize_helper (Str.string_after input 3) (acc @ [Tok_Not]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match boolofboolRegex input 0 then let matched = check_if_ID input (Str.matched_string input) in
    if matched = "true" then tokenize_helper (Str.string_after input 4) (acc @ [Tok_Bool true]) else
    if matched = "false" then tokenize_helper (Str.string_after input 5) (acc @ [Tok_Bool false]) else
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match intofintRegex input 0 then let matched = Str.matched_string input in
    if String.contains matched '(' then
      tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_Int (int_of_string (String.sub matched 1 ((String.length matched) - 2)))])
    else
      tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_Int (int_of_string matched)]) else
  if Str.string_match stringofstringRegex input 0 then
    let matched = trim_quotes (Str.matched_string input) true in
    tokenize_helper (Str.string_after input (String.length (Str.matched_string input))) (acc @ [Tok_String matched]) else
  if Str.string_match idofstringRegex input 0 then let matched = Str.matched_string input in
    tokenize_helper (Str.string_after input (String.length matched)) (acc @ [Tok_ID matched]) else
  if Str.string_match rParenRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_RParen]) else 
  if Str.string_match lParenRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_LParen]) else
  if Str.string_match equalRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Equal]) else
  if Str.string_match greaterRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Greater]) else
  if Str.string_match lessRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Less]) else
  if Str.string_match addRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Add]) else
  if Str.string_match subRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Sub]) else
  if Str.string_match multRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Mult]) else
  if Str.string_match divRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Div]) else
  if Str.string_match concatRegex input 0 then tokenize_helper (Str.string_after input 1) (acc @ [Tok_Concat]) else
  raise (InvalidInputException ("Unrecognized token: " ^ String.make 1 (String.get input 0)))

let tokenize input = tokenize_helper input []
