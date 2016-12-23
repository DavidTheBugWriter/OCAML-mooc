 (* 

The given prelude defines three types, one for three dimensional points, another 
for velocity vectors in three dimensions, and another one representing moving objects in space.

1. Write a function move : point -> dpoint -> point such that move p dp is the 
point p whose coordinates have been updated according to dp. 
(x is now x +. dx, y is now y +. dy, z is now z +. dz.

2. Write a function next : physical_object -> physical_object such that next o is 
the physical object o at time t + dt. 
The position of next o is the position of o moved according to its velocity vector.

3. Suppose that these objects are spheres whose radius is 1.0. 
Write a function will_collide_soon : physical_object -> physical_object -> bool that 
tells if at the next instant, the two spheres will intersect.


TIME ON PLANET SHADOKUS  (30 points possible)
On planet Shadokus, a year has 5 months, each month has 4 days, each day has 3 hours and each hour has 2 minutes. A calendar date is therefore defined as the record type date of the given prelude.

A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and its minute index is >= 0 and <= 1. 
The start of year 12 would be:
{ year = 12; month = 1; day = 1; hour = 0; minute = 0 }
The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 }

Write a function wellformed : date -> bool which checks that the input date is well formed.
On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a magical computer that evaluates the infinite lambda-term of time. It is defined by value the_origin_of_time of the given prelude. 
Write a function next : date -> date which computes the date which comes one minute after the input date.
In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time). 
Write a function of_int : int -> date that converts such an integer into a date.
THE GIVEN PRELUDE

type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

*)


type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint } ;;

1.
let move p dp =
  {x = p.x +. dp.dx; y = p.y+.dp.dy; z = p.z+.dp.dz };;

2.
e.g.: point1 = {1.0; 1.0; 1.0}
dpoint = {0.5; 0.1; 0.6} 
po1 = {point1; dpoint} (dpoint is the displacement *not* the velocity contrary to text above.
po2 = point1 + dpoint = {1.5; 1.1; 1.6}

let next obj =  
    let old_position = obj.position in
    let dp = obj.velocity in
    let newpos = move old_position dp in
    { position = newpos; velocity = dp };;

3.

Spheres intersect if any of x or y or z coordinates overlap for radius of 1.0.
(* intersect:  x1l = 1.0 x1u=3.0 & x2l = 2.0 & x2u=3.0 / dont intersect" x1l = 0 x1u = 3.0 & x2l = 4.0 & x2u - 5.0 *)

let dimension_intersect n1 n2 =
  let n1l = n1 -. 0.5 and
  n1u = n1 +. 0.5 and
  n2l = n2 -. 0.5 and
  n2u = n2 +. 0.5 in
    n2l <= n1u && n2u >= n1l;; 
  
let object_intersect o1 o2 =
  let a = o1.position and
  b = o1.position in
    dimension_intersect a.x b.x && dimension_intersect a.y b.y && dimension_intersect a.z b.z ;;  

let will_collide_soon p1 p2 =
  let ob1_next = next p1 and
  ob2_next = next p2 in
    object_intersect ob1_next ob2_next ;; 


    =====
MOOC website response:
    found will_collide_soon with compatible type.
Computing
will_collide_soon
  {position = {x = 0.762; y = -1.570; z = -2.023};
   velocity = {dx = -0.048; dy = 0.208; dz = -0.117}}
  {position = {x = 1.418; y = -0.434; z = -1.735};
   velocity = {dx = 0.558; dy = -0.252; dz = 0.589}}
1 pt Correct value true
Computing
will_collide_soon
  {position = {x = 2.335; y = -0.195; z = 1.624};
   velocity = {dx = -0.457; dy = -0.857; dz = 0.920}}
  {position = {x = 1.244; y = -1.305; z = 1.933};
   velocity = {dx = 0.817; dy = 0.959; dz = 0.839}}
1 pt Correct value true
Computing
will_collide_soon
  {position = {x = -1.694; y = 1.072; z = -1.769};
   velocity = {dx = 0.787; dy = -0.501; dz = -0.919}}
  {position = {x = 0.228; y = -0.569; z = 1.656};
   velocity = {dx = 0.767; dy = -0.261; dz = -0.994}}
0 pt Wrong value true
Computing
will_collide_soon
  {position = {x = 2.007; y = 0.479; z = 0.797};
   velocity = {dx = 0.002; dy = -0.376; dz = -0.423}}
  {position = {x = 2.001; y = 1.489; z = 1.946};
   velocity = {dx = 0.132; dy = 0.310; dz = 0.525}}
Wrong value true
Computing
will_collide_soon
  {position = {x = 1.231; y = 1.401; z = 0.138};
   velocity = {dx = 0.180; dy = 0.235; dz = -0.835}}
  {position = {x = 1.193; y = 2.339; z = -1.447};
   velocity = {dx = -0.471; dy = -0.151; dz = 0.551}}
1 pt Correct value true
Computing
will_collide_soon
  {position = {x = 0.571; y = -1.666; z = -0.183};
   velocity = {dx = 0.080; dy = -0.132; dz = 0.370}}
  {position = {x = 1.608; y = 1.750; z = 0.791};
   velocity = {dx = 0.525; dy = -0.306; dz = 0.918}}
0 pt Wrong value true
Computing
will_collide_soon
  {position = {x = -1.728; y = -1.170; z = 0.714};
   velocity = {dx = -0.852; dy = -0.519; dz = -0.918}}
  {position = {x = -2.092; y = -1.682; z = 1.074};
   velocity = {dx = 0.623; dy = 0.785; dz = -0.033}}
1 pt Correct value true
Computing
will_collide_soon
  {position = {x = 1.684; y = -1.260; z = -2.207};
   velocity = {dx = 0.072; dy = -0.400; dz = 0.020}}
  {position = {x = -0.348; y = -1.127; z = 0.778};
   velocity = {dx = -0.095; dy = 0.932; dz = 0.851}}
0 pt Wrong value true
Computing
will_collide_soon
  {position = {x = -0.442; y = 0.525; z = 0.660};
   velocity = {dx = 0.069; dy = 0.049; dz = 0.208}}
  {position = {x = -0.426; y = -1.218; z = 0.641};
   velocity = {dx = 0.532; dy = -0.478; dz = -0.906}}
0 pt Wrong value true
Computing
will_collide_soon
  {position = {x = 0.770; y = -1.388; z = 0.137};
   velocity = {dx = -0.038; dy = 0.967; dz = -0.548}}
  {position = {x = 0.065; y = -0.536; z = 0.983};
   velocity = {dx = 0.775; dy = -0.022; dz = 0.829}}
0 pt Wrong value true   