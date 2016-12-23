(*

In this exercise, we implement a queue with a pair of two lists (front, back) such 
that front @ List.rev back represents the sequence of elements in the queue.

1.
Write a function is_empty : queue -> bool such that is_empty q is true if 
and only if q has no element.

2.
Write a function enqueue : int -> queue -> queue such that enqueue x q is the 
queue as q except that x is at the end of the queue.

3.
Write a function split : int list -> int list * int list such that split l = (front, back) 
where l = back @ List.rev front and the length of back and 
front is List.length l / 2 or List.length l / 2 + 1

The first paragraph does not say l =, it says a queue is a pair of list in a tuple (front, back). 
So if the following list of items has the sequence x1,x2,x3,x4,x5,x6 and is stored 
in a list [x1;x2;x3;x4;x5;x6], the resulting queue structure resemble ([x1;x2;x3],[x6;x5;x4]).
If you collected the items as they arrived into a list you would have the list in the 
reverse order [x6;x5;x4;x3;x2;x1]. The split function takes the list in the reverse order and 
returns the proper queue structure of ([x1;x2;x3],[x6;x5;x4]).


4.
Write a function dequeue : queue -> int * queue such that 
dequeue q = (x, q') where x is the front element of the queue q and q' 
corresponds to remaining elements. This function assumes that q is non empty.

*)

let is_empty (front, back) =
    match (front, back) with
    |([],[]) -> true
    | _ -> false ;;

(* 4 ([4; -5; -2; 2], [4; 0; -1; -3; -4; -5; -3; 0; 1]) -> ?
Computing enqueue 2 ([], [])
Correct value: ([2], [])

Computing enqueue -1 ([-3; -1; 2; 0; -4; 1; -5; -3; -4], [4; -1; 4])
Correct value ([-3; -1; 2; 0; -4; 1; -5; -3; -4], [-1; 4; -1; 4])
*)    
 let enqueue x (front, back) =
    (front, [x] @ back);; 

let enqueue x (front, back) =
 (front, x::back)   ;; 


(* l= [x6,x5,x4,x3,x2,x1] q=([x1;x2;x3],[x6;x5;x4]) = (front, back) *)
let rec chomp n acc = function
    | [] -> acc, [] 
    | x::tail when n >0 ->
        chomp (n-1) (x::acc) tail
    | x::tail as l ->
       List.rev l, List.rev acc in;;

let split l =
    let rec chomp n acc = function
        | [] -> acc, [] 
        | x::tail when n >0 ->
            chomp (n-1) (x::acc) tail
        | x::tail as l ->
        List.rev l, List.rev acc in
    let len = (List.length l)/2 in
    chomp len [] l ;;

(* dequeue : queue -> int * queue 

dequeue ([], [3; 2; 1]) = = (1, ([2; 3], []))
*)

let dequeue (front, back) =    
    let hd::tail = front in
    (hd, (tail, back)) ;;

let dequeue (front, back) =    
    match (front, back) with 
    | [], q ->
        let hd::q' = List.rev q in
        (hd, (q', []))
    | _ ->
        let hd::tail = front in
        (hd, (tail, back)) ;;