integer_square_root that calculates the integer square root of a 
positive integer n, that is the largest integer r such that r * r <= n. 
Hint: you may use floating point arithmetic, but don't forget that 
you have to convert explicitely between float and int.
hint 2: Google for Babylonian method

let integer_square_root n =

 let rec sqrt n = 
    if n = 1 then n else 
    let guess = float_of_int n /. 2. in
    let x = 0.5 *. (guess +. (float_of_int n /. guess)) in
    if x *. x > float_of_int n  
    then 
        (*Printf.printf "x: %d n: %d \n" x n ; *)
        sqrt (int_of_float x) 
    else 
        (Printf.printf "x: %f  \n" x;
        int_of_float x) ;;

let integer_square_root n =  
    let guess = n / 2 in (* dont bother with a rough estimation method - just use 1/2 *)
    sqrt2 guess n;; 

 let rec sqrt2 guess target = 
    let x = (guess + (target/guess))/2 in 
    if x * x <= target  
    then 
        x
    else
        sqrt2 x target ;;
     
       
