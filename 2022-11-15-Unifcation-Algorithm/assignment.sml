signature SIGNATURE = sig
    type symbol   
    val arity   : symbol -> int
    val compare : symbol * symbol -> order 
end

structure TypeSig: SIGNATURE = struct
    datatype symbol =   int
                    |   bool
                    |   arrow

    fun     arity (int)     =  0
        |   arity (bool)    =  0
        |   arity (arrow)   =  1
    
    fun     compare (int, int)      = EQUAL
        |   compare (bool, bool)    = EQUAL
        |   compare (arrow, arrow)  = EQUAL
        |   compare (bool, int)     = LESS
        |   compare (int, arrow)    = LESS
        |   compare (bool, arrow)   = LESS
        |   compare (arrow, int)    = GREATER
        |   compare (int, bool)     = GREATER
        |   compare (arrow, bool)   = GREATER
end

fun     zip (x::xs) (y::ys) = ((x,y)::(zip xs ys))

functor Unify (S : SIGNATURE) = struct

    datatype term   = Var of Atom.atom
                    | Apply of S.symbol * term list

    type telescope  = term AtomMap.map 
    type equation   = term*term

    fun unify (tel:telescope) (eq:equation) = 
                    case eq of
                            (Var x,t) => AtomMap.singleton(x, t)
                        |   (s, Var y) => AtomMap.singleton(y, s)
                        |   (Apply (f,fargs), Apply (g, gargs)) => unifyList tel (zip fargs gargs)

    and unifyList (tel:telescope) [] = tel
    |   unifyList (tel:telescope) ((s,t)::eqs) = unifyList (unify tel (s,t)) eqs 
end