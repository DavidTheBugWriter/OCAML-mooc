(*

The previous week, we asked you the following question: Consider a non empty array of integers a, 
write a function min_index : int array -> int that returns the index of the minimal element of a. 
As the arrays contain integers and the indices of arrays are also represented by integers, you 
might have confused an index and the content of a cell. To avoid such a confusion, let us define 
a type for index (given in the prelude below). 
This type has a single constructor waiting for one integer. 
For instance, if you want to represent the index 0, use the value Index 0. 
Defining such a type is interesting because it allows the type-checker to check that an 
integer is not used where an index is expected (or the converse).

1.
Write a function read : int array -> index -> int such that read a (Index k) returns the 
k-th element of a.

2.
Write a function inside : int array -> index -> bool such that inside a idx is true 
if and only if idx is a valid index for the array a.

3.
Write a function next : index -> index such that next (Index k) is equal to Index (k + 1).

4.
Consider a non empty array of integers a, write a function min_index : int array -> index 
that returns the index of the minimal element of a.

THE GIVEN PRELUDE

type index = Index of int

*)

(* read : int array -> index -> int
read a (Index k) -> a[k]
 *)

(* 1. *)
let read a k =
    let x = match k with 
    | Index x -> x in     
    a.(x) ;;  

(* 2. *)
let inside a index =
    let len = Array.length(a) in
    match index with
    | Index x when x >=0 && x<=(len-1) -> true
    | _ -> false ;;

(* 3. *)
let next index =
    match index with
    | Index x -> Index (x+1) ;;

(* 4. *)

(* 

let min_0 a =
  let rec min2 ary idx (icand, candidate) = 
    let length = Array.length(ary) 
    in 
    if length = 1 || idx >= length then (icand, candidate) else 
    match ary.(idx) < candidate with
    | true  -> min2 ary (idx+1) (idx, ary.(idx)) 
    | _ -> min2 ary (idx+1) (icand, candidate)  
  in
  min2 a 0 (0, a.(0)) ;;

let min a = 
  let (cand, minint) = min_0 a in
  minint ;;

*)

let min_0 a =
  let rec min2 ary idx (icand, candidate) = 
    let get_index = function |Index x -> x in 
    let idx' = get_index idx in
    let length = Array.length(ary) 
    in 
    if length = 1 || idx' >= length then (icand, candidate) else 
    match ary.(idx') < candidate with
    | true  -> min2 ary (Index (idx'+1)) (idx, ary.(idx')) 
    | _ -> min2 ary (Index(idx'+1)) (icand, candidate)  
  in
  min2 a (Index 0) (Index 0, a.(0)) ;;

  let min_index a = 
  let (cand, minint) = min_0 a in
  cand ;;

(*

Write a function mem : int -> int list -> bool such that mem x l is true if and only if x occurs in l.

*)
mem : int -> int list -> bool

let rec mem x l = 
match l with
  | h::t  ->
      if x = h then true else mem x t
  | [] ->
    false ;;  
  

  (*

  Write a function append : int list -> int list -> int list such that append l1 l2 is the concatenation of l1 and l2
  *)
let rec append l1 l2 =
  l1 @ l2 ;;
  
(*
Write a function combine : int list -> int list -> (int * int) list such that combine l1 l2 is the 
list of pairs obtained by joining the elements of l1 and l2. This function assumes that l1 and l2 have 
the same length. For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)].
*)

let rec combine l1 l2 =
    match l1, l2 with
    | h::t, h'::t' ->
      [(h, h')] @ combine t t'
    | _ ->
      [] ;;

(*
Write a function assoc : (string * int) list -> string -> int option such that 
assoc l k = Some x if (k, x) is the first pair of l whose first component is k. 
If no such pair exists, assoc l k = None.

assoc
  [("as", 13); ("ocp", 14); ("struct", 28); ("if", 74); ("let", 16);
   ("mod", 24)] "as" => Some 13 
assoc [("as", 39); ("for", 73); ("match", 75)] "open" => None (correct!) ;
*)

let rec assoc l k = 
match l with
| (k', x)::_ when k'=k ->
  Some x
| (k', x)::t -> assoc t k
| _ -> None ;;