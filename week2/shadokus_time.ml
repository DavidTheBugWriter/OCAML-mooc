
(* 

On planet Shadokus, a year has 5 months, each month has 4 days, each day 
has 3 hours and each hour has 2 minutes. A calendar date is therefore defined 
as the record type date of the given prelude.

1.
A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, 
its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, 
and its minute index is >= 0 and <= 1. 

The start of year 12 would be:
{ year = 12; month = 1; day = 1; hour = 0; minute = 0 }
The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 }

Write a function wellformed : date -> bool which checks that the input date is well formed.

2.
On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a 
magical computer that evaluates the infinite lambda-term of time. It is defined 
by value the_origin_of_time of the given prelude. 
Write a function next : date -> date which computes the date which comes 
one minute after the input date.

3.
In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time). 
Write a function of_int : int -> date that converts such an integer into a date.

*)

type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }
(* ====    
1. 
    match date with 
    | {year; month; day; hour; minute} when 
    year > 0 && month >=1 && month <=5 && day >=1 && day <=4 && hour >= 0 && hour <=2 && minute>=0 && minute <=1  -> true 
    | _ -> false;;
*)
   (*
   let iza a = a > 1 ;;
    match s with | {a; b; c} when iza (a) -> true
    |_ -> false ;;
    *)

let next date = 
    let rec flipdate date monthflip =  
      if not monthflip then
            let {year; month; day; hour; minute} = date in
            let minute = minute + 1 
            in
            match (year, month, day, hour, minute) with
            | (_,_,_,_, minute) when minute > 1 -> flipdate {year; month; day; hour = hour+1; minute = -1} false 
            | (_,_,_,hour,_) when hour > 2 -> flipdate {year; month; day = day+1; hour = 0; minute = -1} false 
            | (_,_,day,_,_) when day > 4 -> flipdate {year; month = month+1; day = 1 ; hour = 0; minute = -1} false 
            | (_,month,_,_,_) when month > 5 -> flipdate {year = year+1; month = 1; day = 1 ; hour= 0; minute = -1} (monthflip = true)
            | _ -> {year; month; day; hour ; minute} 
       else
        date     
    in 
    flipdate date false
    ;;

(* 3. *)
(* A year has 5 * 4 * 3 * 2 = 120 minutes/month has 24 minutes/day has 6 minutes /hour has 2 minutes 
int_count mod 120 & int_count / 120
year  
*)


let rec fact x =
    if x <= 1 then 1 else x * fact (x - 1);;


let rec of_int2 adate power rem : date = 
    Printf.printf " power: %d rem: %d\n" power rem;
    if power <= 1 then 
        {year = adate.year; month = adate.month; day = adate.day; hour = adate.hour; minute = adate.minute+rem}
    else
        let divisor = fact power 
        in
        let quotient, rem' = rem/divisor, rem mod divisor  
        in
        match power with
        | 5 -> of_int2 {year = adate.year + quotient; month = adate.month; day = adate.day; hour = adate.hour; minute = adate.minute} 4 rem'
        | 4 -> of_int2 {year = adate.year; month = adate.month + quotient; day = adate.day; hour = adate.hour; minute = adate.minute} 3 rem'
        | 3 -> of_int2 {year = adate.year; month = adate.month; day = adate.day + quotient; hour = adate.hour; minute = adate.minute} 2 rem'
        | 2 -> of_int2 {year = adate.year; month = adate.month; day = adate.day; hour = adate.hour + quotient; minute = adate.minute} 1 rem'
        | 1 -> of_int2 {year = adate.year; month = adate.month; day = adate.day; hour = adate.hour; minute = adate.minute + quotient} 0 rem'
        | _ -> {year = adate.year; month = adate.month; day = adate.day; hour = adate.hour; minute = adate.minute}
        ;; 

let of_int minutes =
    of_int2 the_origin_of_time 5 minutes ;;

#trace of_int2 ;;