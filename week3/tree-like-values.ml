(* 

type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp ;;

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

*)
(* Write the expression 2 * 2 + 3 * 3 in a variable my_example *)

let my_example =
 EAdd( EMul(EInt 2, EInt 2), EMul( EInt 3, EInt 3) );;


(*
Write a function eval : exp -> int that computes the value of an arithmetic expression. The evaluation rules are:
If the expression is an integer x, the evaluation is x.
If the expression is lhs + rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x + y.
If the expression is lhs * rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x * y.
*) 

let eval e = 
  let rec eval2 e = 
    match e with
    | EInt x ->
      x
    | EAdd (EInt x, EInt y) ->
      x+y
    | EMul (EInt x, EInt y) ->
      x*y
    | EAdd (left, right) ->
      eval2 left + eval2 right
    | EMul (left,right) ->
      eval2 left * eval2 right 
  in
  eval2 e ;; 


(*
If an expression is of the form a * b + a * c then a * (b + c) is a factorized equivalent expression. 
Write a function factorize : exp -> exp that implements this transformation on its input exp if it has 
the shape a * b + a * c or does nothing otherwise.

The symbols (a, b, c and e) can match any expressions, not just integers.
these are a syntactical rewritings, so two expressions are considered equal if and only if they are 
exactly the same expressions (simply use the = operator to check that).
The rewritings have to be done on the first level of the expression only, not recursively 
and not deeper in the expression. If the toplevel expression does not match the expected 
pattern, simply return the expression untouched.

*)

let factorize e = 
if e = EAdd(EMul (a, b), EMul(a,c)) then EMul(a, EAdd(b,c)) else e ;;

let factorize e = 
  match e with
  | EAdd(EMul (a, b), EMul(a',c)) when a=a'->
    EMul(a, EAdd(b,c))
  | _ -> e ;;


(*
Write the reverse transformation of factorize, expand : exp -> exp, which turns an 
expression of the shape a * (b + c) into a * b + a * c.
*)

let expand e =
match e with 
| EMul(a, EAdd(b,c)) ->
  EAdd(EMul (a, b), EMul(a,c))
| _ -> e ;;


(*
Implement a function simplify: exp -> exp which takes an expression e and:
If e is of the shape e * 0 or 0 * e, returns the expression 0.
If e is of the shape e * 1 or 1 * e, returns the expression e.
If e is of the shape e + 0 or 0 + e, returns the expression e.
and does nothing otherwise.

simplify
  (EMul
    (EAdd (EMul (EMul (EInt 4, EInt 0), EInt 1),
      EAdd (EMul (EInt (-6), EInt (-9)), EInt 5)),
    EInt 2))

*)

let simplify e = 
match e with
| EMul (_,EInt 0) -> EInt 0
| EMul(EInt 0, _) -> EInt 0
| EMul (e, EInt 1) -> e 
| EMul (EInt 1, e) -> e 
| EAdd (e, EInt 0) -> e
| EAdd (EInt 0, e) -> e 
| _ -> e;;