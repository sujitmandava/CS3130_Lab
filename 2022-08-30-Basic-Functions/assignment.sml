(* 
    Q1. Write the tri-variate versions of curry and uncurry 
    curry : ('a*'b*'c -> 'd) -> ('a->'b->'c'>'d);
    uncurry : ('a->'b->'c'>'d) -> ('a*'b*'c -> 'd);
*)

fun curry f x y z = f (x,y,z)
fun uncurry f (x,y,z) = f x y z

(*
    Q2. Functions to project tuple into components 
*)

fun fst (x,y) = x
fun snd (x,y) = y

(* 
    Q3. Find length of a list 
*)

fun length [] = 0
    | length (x::xs) = 1 + length(xs);

(* 
    Q4. Reverse a list 
    Time complexity O(n)
*)

fun rev [] [] = []
|   rev [] (y::ys) = rev [y] ys
|   rev ys [] = ys
|   rev ys (x::xs) = rev (x::ys) xs

fun reverse k = rev [] k;

(* 
    Q5. Define a function to compute nth fibonacci number 
*)

fun helper 0 x y = y
    |   helper n f1 f0 = helper(n-1) (f1+f0) f1

fun fib n = helper n 1 0;
