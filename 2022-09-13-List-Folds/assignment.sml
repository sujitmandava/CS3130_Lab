(*
    1. Define the functions foldr and foldl using the pattern matching
    for list.
*)

fun     foldl (f: 'a*'b->'b) (s0: 'b) ([]: 'a list) = s0
    |   foldl (f: 'a*'b->'b) (s0: 'b) (x::xs: 'a list) = foldl f (f(x,s0)) xs

fun     foldr (f: 'a*'b->'b) (s0: 'b) ([]: 'a list) = s0
    |   foldr (f: 'a*'b->'b) (s0: 'b) (x::xs: 'a list) = f(x, (foldr f s0 xs))

(*
    2. Without using pattern matching, define the function sum : int list -> int that computes the sum of a list of integers.
*)

fun     sum(x,y) = x+y

fun     sumList (l: int list) = foldl sum 0 l

(*
    3. Instead of using explicit recursion, define the following library
    function in terms of either foldr or foldl which ever is
    convenient. For the documentation of these library function, read the
    documentation of the List structure

        partition : ('a -> bool) -> 'a list -> 'a list * 'a list
        map : ('a -> 'b) -> 'a list -> 'b list.
        reverse : 'a list -> 'a list
        nth : 'a list * int -> 'a option
*)


fun assignVal (f: 'a->bool) ((x: 'a), ((xt, xf): 'a list*'a list))=
        case (f x) of
        true=> (x::xt, xf)
    |   false=> (xt, x::xf)   

datatype 'a Find = LookingFor of int
                 | Found      of 'a

fun     nthfun (x, LookingFor j) = if (j=0) then Found x else LookingFor (j-1)
    |   nthfun (x, Found y) = Found y

fun nthAux (l, i) = foldl nthfun (LookingFor i) l

fun     partition (f: 'a->bool) (l: 'a list) = foldl (assignVal f) ([]: 'a list, []: 'a list) l  
fun     map (F: 'a->'b) (l: 'a list) = foldl (fn (x: 'a, acc: 'b list) => (F x)::acc) [] l
fun     reverse (l: 'a list) = foldl (fn (x: 'a, acc: 'a list) => x::acc) [] l

fun nth (l,i) = 
        case nthAux(l,i) of 
        LookingFor k => NONE
    |   Found x => SOME x