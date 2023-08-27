(*
    Q1. Write the quick sort algorithm as a functor that takes the ordering
   through its input structure O (see below).
*)

signature SORT = sig
    type t
    val sort : t list -> t list
end

signature ORD_KEY = sig
    type ord_key

    val compare : ord_key * ord_key -> order

end 

functor QSort ( O : ORD_KEY ) : SORT = struct
    type t = O.ord_key

    (* fun qpartition (arr: O.ord_key list) = List.partition (fn x => O.compare(x,(List.hd arr)) = LESS) arr *)
    (* (fn x => O.compare(x,(List.hd arr)) *)

    fun cmp x y = O.compare(x,y) = LESS

    fun     sort [] = []
        |   sort (a::arr) = let
                            val (gte, lt) = List.partition (cmp a) arr
                        in
                            (sort lt) @ [a] @ (sort gte)
                        end
end


structure IntOrd: ORD_KEY = struct
    type ord_key = int
    fun compare (x: ord_key, y: ord_key) = Int.compare(x,y)
end

structure intQSort: SORT = QSort (IntOrd)

(* val sorttt = intQSort.sort *)

val sortedList = intQSort.sort([3,2,1,4])

(* open intQSort *)