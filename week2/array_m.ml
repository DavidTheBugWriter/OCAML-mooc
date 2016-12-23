(* Consider a non empty array of integers a.

Write a function min : int array -> int that returns the minimal element of a.
Write a function min_index : int array -> int that returns the index of the minimal element of a.

1.
let min a = 
2.
let min_index a =
  "Replace this string with your implementation." ;;


Write a function is_sorted : string array -> bool which checks if the values of the input array are 
sorted in strictly increasing order, implying that its elements are unique (use String.compare).
Using the binary search algorithm, an element can be found very quickly in a sorted array. 

Write a function find : string array -> string -> int such that find arr word is the index of the word 
in the sorted array arr if it occurs in arr or -1 if word does not occur in arr. 
The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. 
Beware that you really perform the minimal number of accesses. For instance, if your function has to 
test the contents of a cell twice, be sure to put the result of the access in a variable, and then 
perform the tests on that variable.

*)

let rec pp_array a =
  let rec pp_loop a idx =
    let len = Array.length(a) in 
    match idx<=len-1 with 
    |true ->
      Printf.printf "%d; " a.(idx);
      pp_loop a (idx+1) 
    | _ ->
      Printf.printf ""
    in
  pp_loop a 0 ;;  

(*
let min a =
  let rec min2 ary idx candidate = 
    let length = Array.length(ary) 
    in 
    if length = 1 || idx >= length then candidate else 
    match ary.(idx) < candidate with
    | true  -> min2 ary (idx+1) ary.(idx) 
    | _ -> min2 ary (idx+1) candidate  
  in
  min2 a 0 a.(0) ;;
*)

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

  let min_index a = 
  let (cand, minint) = min_0 a in
  cand ;;

(*

1. Write a function is_sorted : string array -> bool which checks if the values of the input array 
are sorted in strictly increasing order, implying that its elements are unique (use String.compare).

2. Using the binary search algorithm, an element can be found very quickly in a sorted array. 
Write a function find : string array -> string -> int such that find arr word is the index 
of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr. 
The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. 
Beware that you really perform the minimal number of accesses. For instance, if your function has to 
test the contents of a cell twice, be sure to put the result of the access in a variable, and then 
perform the tests on that variable.
*)


let rec sort ary srt_cand = 
    let array_len = Array.length(ary) in
    let array_end = array_len-1   
    in 
    match ary with
    | [||] -> srt_cand
    | [|_|] -> srt_cand
    | [|a; b|] when a >= b -> false
    | [|a; b|] when a <b -> true
    | _ ->   
      let sub = Array.sub ary 0 (array_len-1) in
      if ary.(array_end-1) >= ary.(array_end) 
        then false
        else  
          sort sub true;;

let rec is_sorted a = 
  sort a true ;;


(* find : string array -> string -> int 
find [|"bar";"foo"|] "for";;  (* 'for' not found in array *)
find [|"abg";"def";"sok"|] "sok" ;; 
find [|"abg";"def";"sok";"zt"|]  ;; 
find [|"01w";"abg";"def";"sok";"zt"|] "abg" ;;

*)

let rec find' dict word low high = 
  if high < low then 
     -1 
  else
    let midpoint = low+(high-low)/2 in
    match (String.compare dict.(midpoint) word) with
    | 0 ->
      midpoint 
    | -1 -> (* midpoint < word *)
      find' dict word (midpoint+1) high 
    | 1 ->  (* midpoint > word *)
      find' dict word low (midpoint-1)  ;;

let rec find dict word = 
  let high = Array.length(dict) in
  find' dict word 0 (high-1) ;;
