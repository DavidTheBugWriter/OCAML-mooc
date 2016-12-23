(* 
Write a function exchange of type int -> int that takes an integer x between 10 and 99 
and returns an integer which is x whose digits have been exchanged. For instance, exchange 73 = 37.

Enigma: If you multiply my grand-son age by four, you know how old I am. Now, if you exchange 
the two digits of our ages then you have to multiply by three my age to get the age of my grand-son!

grandsons age * 4 = my age. 
Swap digits of our ages. multiply his_swapped by 3 to get mine_swapped. 
20 & 71 => 02, 17/ compare 02 * 3 to 17;  but 15 * 4 = 60 however for (72,18) 72 = 18*4 & 
when tranposed 27 * 3 = 81 & 81 = 18 transposed.

Define is_valid_answer of type int * int -> bool such that 
is_valid_answer (grand_father_age, grand_son_age) returns true if and only if 
grand_father_age and grand_son_age verify the constraints of the puzzle.

Write a function find : (int * int) -> (int * int) that takes a 
pair (max_grand_father_age, min_grand_son_age) and returns a 
solution (grand_father_age, grand_son_age) to the problem, 
where min_grand_son_age <= grand_son_age < grand_father_age <= max_grand_father_age 
or (-1, -1) if there was no valid answer in the given range.
*)

let exchange k =
  let swap = fun x ->
    let left = x / 10 in
    let right = x mod 10 in
    right*10 + left in
  match k with
  | k when k >= 10 && k <=99 -> swap k  
  | _ -> 0 ;;

let is_valid_answer (grand_father_age, grand_son_age) =
  match grand_father_age with
    | grand_father_age when grand_son_age * 4 <> grand_father_age -> false
    | _ ->
      let gs_tp = exchange grand_son_age in 
      let gf_tp = exchange grand_father_age in
      match gs_tp with
      | gs_tp when gf_tp * 3 = gs_tp -> true
      | _ -> false ;;
      
let rec find (max_gf_age, min_gs_age) =
  let rec find_gs (gf_upper_age, gs_lower_age) =
    if is_valid_answer (gf_upper_age, gs_lower_age) then (gf_upper_age, gs_lower_age) else 
    match gf_upper_age > gs_lower_age with
    | true -> find_gs (gf_upper_age, gs_lower_age+1)
    | false -> (-1, -1) 
  in
  let rec find_gf (gf_upper_age, gs_lower_age) =
    find_gs (gf_upper_age, gs_lower_age)
  in
    match find_gf (max_gf_age, min_gs_age) with
    | (-1, -1) -> find (max_gf_age-1, min_gs_age) 
    | (x, y) -> (x, y) ;;
