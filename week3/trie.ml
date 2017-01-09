(*
Trie trees on Wikipedia:
https://en.wikipedia.org/wiki/Trie

 we will implement such a data structure, assuming that we want to associate integers to the 
 strings of the dictionary. Let us define a trie using two mutually defined types (given in the prelude):
1) trie which represents a trie, that is a tree whose root may contain an integer and whose 
children are indexed by characters ;
2) char_to_children which implements the associative data structure whose keys are characters 
and whose values are trie (childrens).

As a trade-off between speed and memory consumption, we choose an associative list to represent 
the association between characters and children. 
The prelude also gives examples of empty trie and of another one that contains the following pairs (key, value): 
[("A", 15); ("to", 7); ("tea", 3);("ted", 4); ("ten", 12); ("i", 11); ("in", 5); ("inn", 9)].

1. Write a function children_from_char : char_to_children -> char -> trie option such that
a) children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
b) children_from_char m c = None if no such pair exists in m.



2. Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
a) children_from_char (update_children m c t) c = Some t ;
b) children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
c) If children_from_char m c = Some t then List.length (update_children m c t') = List.length m.

(it is a list of doubles with the first data point being a character and the second being a trie 
[(c1,t1);(c2,t2);…;(cn,tn)]. The second parameter is a character. Search the list for the char and 
replace with current trie in the tuple with the third parameter passed to the function. 
If you don't find the character in the list of tuples, then create a new tuple with the second and 
third parameter and add it to the list.

let rec update_children m c t =
m is just a list of pairs [(c1, t1); (c2, t2); ... ] where every ci is a character and every ti is a trie. 
The other two arguments c an t are also a character and a trie respectively.
You should update the list m with the given pair (c, t). If the list is empty, just put the pair (c, t) in
 a list and return it. If the list is not empty, look at the first element (c1, t1).
Now there are two cases. If c1 equals c, you have to substitute t1 with t in the pair (c1, t1), leave the 
rest of the list untouched and return it. Otherwise leave (c1, t1) untouched and cons it to a recursive 
call with the rest of the list.




3. Write a function lookup : trie -> string -> int option such that lookup trie w = Some i if i is the 
value of the key w in trie and lookup trie w = None if w is not a key of trie. 

To look for a key in a trie, iterate over the characters of the key from left to right. Given the 
current character c and the current node of the trie n, look for the children n for character c. 
If such a children exists, continue with that trie and the remainder of the key. If no such children 
exists, the key is not in the trie. When the characters of the key are entirely consumed, look at 
the root of the current trie. If there is an integer, this is the value you are looking for. 
If there is no integer, the key not in the trie.

Write a function insert : trie -> string -> int -> trie such that 
lookup (insert trie w k) w = Some k 
    and 
lookup (insert trie w k) w' = lookup trie w' for w <> w'.

Prelude:  *)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list ;;

let empty =
  Trie (None, []) ;;

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); 
             ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))]) ;;


let example2 =
  Trie (None,   
  [('i', Trie (None, []) )]
  )  ;;


let rec children_from_char m c =
    match m with
    | (c', x)::_ when c'=c ->
        Some x
    | (c', x)::t -> children_from_char t c 
    | _ -> None ;;


(*
let root = Trie (None, []) ;;
let char2ch = [('c',root)] ;;
let char2ch2 = char2ch@char2ch ;;
let (c,m)::tail = char2ch2 ;;  c -> 'c',  m->Trie (None, []), tail -> [('c', Trie (None, []))]
(c,m)::tail ;; ->  [('c', Trie (None, [])); ('c', Trie (None, []))]

let m = cfc ;;
let m2= cfc@cfc ;;

update_children char2ch 'd' root ;; ->  [('d', Trie (None, []))]

update_children char2ch 't' example ;;

let trie1 = Trie (Some 3, []) ;;
let trie2 = Trie (Some 4, []) ;;
let trie3 = Trie (Some 5, []) ;;
let trie4 = Trie (Some 6, []) ;;
let char2ch3 = [('i', trie1);('j', trie2)] ;;
let char2ch4 = [('i', trie1);('j', trie2); ('K', trie3); ('l', trie4)] ;;
 
*)

let rec update_children m c t =
      match m with
      | [] ->
          [(c, t)] 
      | (c', t')::tail when c' = c ->
          [(c',t)]@tail
      | (c',t')::tail  ->
        (c',t')::update_children tail c t ;;

let root = Trie (None, []) ;;
let char2ch = [('c',root)] ;;
update_children [] 'o' root;; (* ->  [('o', Trie (None, []))]  *)
update_children [] 'o' trie1;; (* ->  [('o', Trie (Some 3, []))] *)
let char2ch3 = [('i', trie1);('j', trie2)] ;;
update_children char2ch3 'j' trie2 ;; (* -> [('i', Trie (Some 4, [])); ('j', Trie (Some 4, []))] *)
update_children char2ch4 'K' trie4 ;;

