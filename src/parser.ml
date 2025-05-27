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
  | None -> raise (InvalidInputException "empty token list")
  | Some tok -> (match tok with 
                | Tok_Let -> parse_let toks
                | Tok_If -> parse_if toks
                | Tok_Fun -> parse_fun toks
                | _ -> parse_or toks)

and parse_let toks = 
   let t = match_token toks Tok_Let in
   let is_rec = (if lookahead t = Some Tok_Rec then true else false) in
   let t = if is_rec then match_token t Tok_Rec else t in
   match lookahead t with
   | None -> raise (InvalidInputException "Empty after rec")
   | Some tok -> match tok with 
      | Tok_ID(value) -> (let t' = match_token t (Tok_ID(value)) in
                        match lookahead t' with
                        | None -> raise (InvalidInputException "No equal after ID")
                        | Some tok -> match tok with 
                          | Tok_Equal -> (let t'' = match_token t' Tok_Equal in
                                        let (t3, expr) = parse_expr t'' in
                                        match lookahead t3 with
                                        | None -> raise (InvalidInputException "No expression after equal")
                                        | Some tok -> match tok with
                                            | Tok_In -> (let t4 = match_token t3 Tok_In in
                                                        let (t5, expr') = parse_expr t4 in
                                                        (t5, Let(value, is_rec, expr, expr')))
                                            | _ -> raise (InvalidInputException "No in token in let expression"))
                          | _ -> raise (InvalidInputException "No Equal in let expression"))
      | _ -> raise (InvalidInputException "No ID after let")

and parse_if toks = 
  let t = match_token toks Tok_If in
  let (t', expr1) = parse_expr t in
  match lookahead t' with
  | None -> raise (InvalidInputException "No expression after if")
  | Some tok -> match tok with
      | Tok_Then -> (let t'' = match_token t' Tok_Then in
                    let (t3, expr2) = parse_expr t'' in
                    match lookahead t3 with
                    | None -> raise (InvalidInputException "No expression")
                    | Some tok -> match tok with
                        | Tok_Else -> (let t4 = match_token t3 Tok_Else in
                                      let (t5, expr3) = parse_expr t4 in
                                      (t5, If(expr1, expr2, expr3)))
                        | _ -> raise (InvalidInputException "No \"else\" in if/then expr"))
      | _ -> raise (InvalidInputException "No \"then\" in if/then expression")

and parse_fun toks = 
  let t = match_token toks Tok_Fun in
  match lookahead t with
  | None -> raise (InvalidInputException "No token after fun")
  | Some tok -> match tok with  
      | Tok_ID(value) -> (let t' = match_token t (Tok_ID(value)) in
                        match lookahead t' with
                        | None -> raise (InvalidInputException "No arrow after ID") 
                        | Some tok -> match tok with 
                            | Tok_Arrow -> (let t'' = match_token t' Tok_Arrow in
                                            let (t3, expr) = parse_expr t'' in
                                            (t3, Fun(value, expr)))
                            | _ -> raise (InvalidInputException "No arrow present in fun"))
      | _ -> raise (InvalidInputException "No ID following fun")


and parse_or toks = 
  let (t, and') = parse_and toks in
  match lookahead t with
  | None -> (t, and')
  | Some tok -> match tok with 
                | Tok_Or -> (let t' = match_token t Tok_Or in
                            let (t'', or') = parse_or t' in
                            (t'', Binop(Or, and', or')))
                | _ -> (t, and')

and parse_and toks = 
  let (t, equ) = parse_equality toks in
  match lookahead t with
  | None -> (t, equ)
  | Some tok -> match tok with
                | Tok_And -> (let t' = match_token t Tok_And in
                             let (t'', and') = parse_and t' in
                             (t'', Binop(And, equ, and')))
                | _ -> (t, equ)

and parse_equality toks = 
  let (t, rel) = parse_relational toks in
  match lookahead t with
  | None -> (t, rel)
  | Some tok -> match tok with
                | Tok_Equal -> (let t' = match_token t Tok_Equal in
                               let (t'', equ) = parse_equality t' in
                               (t'', Binop(Equal, rel, equ)))
                | Tok_NotEqual -> (let t' = match_token t Tok_NotEqual in
                                  let (t'', equ) = parse_equality t' in
                                  (t'', Binop(NotEqual, rel, equ)))
                | _ -> (t, rel)

and parse_relational toks = 
  let (t, add) = parse_additive toks in
  match lookahead t with
  | None -> (t, add)
  | Some tok -> match tok with
                | Tok_Greater -> (let t' = match_token t Tok_Greater in
                                 let (t'', rel) = parse_relational t' in
                                 (t'', Binop(Greater, add, rel)))
                | Tok_Less -> (let t' = match_token t Tok_Less in
                              let (t'', rel) = parse_relational t' in
                              (t'', Binop(Less, add, rel)))
                | Tok_GreaterEqual -> (let t' = match_token t Tok_GreaterEqual in
                                      let (t'', rel) = parse_relational t' in
                                      (t'', Binop(GreaterEqual, add, rel)))
                | Tok_LessEqual -> (let t' = match_token t Tok_LessEqual in
                                   let (t'', rel) = parse_relational t' in
                                   (t'', Binop(LessEqual, add, rel)))
                | _ -> (t, add)

and parse_additive toks = 
  let (t, mul) = parse_multiplicative toks in
  match lookahead t with
  | None -> (t, mul)
  | Some tok -> match tok with
                | Tok_Add -> (let t' = match_token t Tok_Add in
                             let (t'', add) = parse_additive t' in
                             (t'', Binop(Add, mul, add)))
                | Tok_Sub -> (let t' = match_token t Tok_Sub in
                             let (t'', add) = parse_additive t' in
                             (t'', Binop(Sub, mul, add)))
                | _ -> (t, mul)

and parse_multiplicative toks = 
  let (t, c) = parse_concat toks in
  match lookahead t with 
  | None -> (t, c)
  | Some tok -> match tok with
                | Tok_Mult -> (let t' = match_token t Tok_Mult in
                              let (t'', mul) = parse_multiplicative t' in
                              (t'', Binop(Mult, c, mul)))
                | Tok_Div -> (let t' = match_token t Tok_Div in
                              let (t'', mul) = parse_multiplicative t' in
                              (t'', Binop(Div, c, mul)))
                | _ -> (t, c)

and parse_concat toks = 
  let (t, u) = parse_unary toks in
  match lookahead t with 
  | None -> (t, u)
  | Some tok -> match tok with
                | Tok_Concat -> (let t' = match_token t Tok_Concat in
                                let (t'', conc) = parse_concat t' in
                                (t'', Binop(Concat, u, conc)))
                | _ -> (t, u)

and parse_unary toks = 
  match lookahead toks with
  | None -> parse_fun_call toks
  | Some tok -> match tok with
                | Tok_Not ->  (let t = match_token toks Tok_Not in
                              let (t', expr) = parse_unary t in
                              (t', Not (expr)))
                | _ -> parse_fun_call toks

and parse_fun_call toks = 
  let (t, p) = parse_primary toks in 
  match lookahead t with 
  | None -> (t, p)
  | Some tok -> (match tok with 
                | Tok_Int(value) -> let (t', expr) = parse_primary t in
                                    (t', FunctionCall (p, expr))
                | Tok_Bool(value) -> let (t', expr) = parse_primary t in
                                      (t', FunctionCall (p, expr))
                | Tok_String(value) -> let (t', expr) = parse_primary t in
                                      (t', FunctionCall (p, expr))
                | Tok_ID(value) -> let (t', expr) = parse_primary t in
                                    (t', FunctionCall (p, expr))
                | Tok_LParen -> let (t', expr) = parse_primary t in
                                (t', FunctionCall (p, expr))
                | _ -> (t, p))
                
                (* FUN TOKEN WILL NOT BE PRESET, NON OPERATOR BETWEEN PRIMARY EXPRESSIONS*)



and parse_primary toks = match lookahead toks with
None -> raise (InvalidInputException "reached none in parse_primary")
| Some tok -> match tok with
            | Tok_Int(value) -> (let t = match_token toks (Tok_Int(value)) in 
                              (t, Value (Int(value))))
            | Tok_Bool(value) -> (let t = match_token toks (Tok_Bool(value)) in 
                                (t, Value(Bool(value))))
            | Tok_String(value) -> (let t = match_token toks (Tok_String(value)) in 
                                  (t, Value(String(value))))
            | Tok_ID(value) -> (let t = match_token toks (Tok_ID(value)) in 
                              (t, ID (value)))
            | Tok_LParen -> (let t = match_token toks Tok_LParen in
                            let (t', exp) = parse_expr t in
                            let t'' = match_token t' Tok_RParen in
                            (t'', exp))
            |_ -> raise (InvalidInputException "Unrecognized Tok")

let rec parse_mutop toks = match lookahead toks with
  | None -> raise (InvalidInputException "empty mutop list")
  | Some tok -> match tok with 
                | Tok_DoubleSemi -> ([], NoOp)
                | Tok_Def -> parse_mutop_def toks
                | _ -> parse_mutop_expr toks

and parse_mutop_def toks = 
  let t = match_token toks Tok_Def in
  match lookahead t with
    | None -> raise (InvalidInputException "expected ID")
    | Some tok -> match tok with 
                  | Tok_ID(value) -> (let t' = match_token t (Tok_ID(value)) in
                        match lookahead t' with
                        | None -> raise (InvalidInputException "expected equal")
                        | Some tok -> match tok with 
                                      | Tok_Equal -> (let t'' = match_token t' Tok_Equal in
                                                      let (t''', exp) = parse_expr t'' in
                                                      match lookahead t''' with
                                                        | None -> raise (InvalidInputException "missing DoubleSemi in def")
                                                        | Some tok -> match tok with 
                                                                      | Tok_DoubleSemi -> (let t4 = match_token t''' Tok_DoubleSemi in
                                                                          if t4 = [] then (t4, Def(value, exp)) else raise (InvalidInputException "tokens after DoubleSemi"))
                                                                      | _ -> raise (InvalidInputException "no semicolon present in def")) 
                                      | _ -> raise (InvalidInputException "no equals after ID"))
                    | _ -> raise (InvalidInputException "no ID after def")

and parse_mutop_expr toks = 
  let (t, exp) = parse_expr toks in
  match lookahead t with 
    | None -> raise (InvalidInputException "no semicolon present in exp")
    | Some tok -> match tok with 
                  | Tok_DoubleSemi -> (let t' = match_token t Tok_DoubleSemi in
                                      if t' = [] then (t', Expr(exp)) else raise (InvalidInputException "tokens after DoubleSemi"))
                  | _ -> raise (InvalidInputException "token other than DoubleSemi present")