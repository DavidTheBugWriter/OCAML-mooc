(*
Optional values are commonly used in OCaml in the return type of partial functions, i.e. functions 
that may fail on some input. The following questions illustrate such situations. 
In the Pervasives module which is loaded automatically, there is a type option with two constructors: 
Some (e) has type 't option if e has type 't and represents the presence of some value e of type 't. 
None has type 't option and represents the absence of some value of type 't.

1.
Write a function find : string array -> string -> int option such that 
find a w = Some idx if a.(idx) = w and find a w = None if there is no such index.
2.
Sometimes, when a value of type t is missing, a default value should be used. 
Write a function default_int : int option -> int such that: default_int None = 0 and default_int (Some x) = x.

3.
Write a function merge : int option -> int option -> int option such that:
merge None None = None
merge (Some x) None = merge None (Some x) = Some x
merge (Some x) (Some y) = Some (x + y) 
*)


let rec find' dict word idx = 
  if idx < 0 then -1 else
    if word = dict.(idx) then idx
    else 
      find' dict word (idx-1)
  ;;


let find a w = 
    let high = Array.length(a) in
    match find' a w (high-1) with 
    | -1 -> None
    | a' -> Some a' ;; 


let default_int opt = 
  match opt with 
  | None -> 0
  | Some x -> x ;;


  let merge a b =
  match (a, b) with 
  | (None, None) -> None
  | (Some a, None) -> Some a 
  | (None, Some a) -> Some a
  | (Some a, Some b) -> Some (a+b) 
  ;;