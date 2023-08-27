(*  
    Q1. Capture the De Bruijn representation of lambda calculus as a SML
        data type.
*)

datatype expDB  =       VarDB of int
                |       Lambda of expDB
                |       ApplyDB of expDB*expDB

(*  
    Q2. Write a program to convert from De Bruijn index to the first order
        representation of lambda calculus expression.
*)

datatype Expr = Var of Atom.atom
            |   Abstract of Atom.atom*Expr
            |   Apply of Expr*Expr

fun     diagonalList [] []                  = [#"x"]
    |   diagonalList [] (x::xs)             = 
            if x=(#"x") then [#"y"] else [#"x"]
    |   diagonalList s []                   = s
    |   diagonalList (x::xs) (y::ys)        = 
            if x=y then (x::(diagonalList xs ys)) else (x::xs)

fun diag s1 s2 = String.implode(diagonalList (String.explode(s1)) (String.explode(s2)))

fun diagA (a1,a2) = Atom.atom(diag (Atom.toString(a2)) (Atom.toString(a1)))

fun fresh s = AtomSet.foldl diagA (Atom.atom("")) s

(* val presentVariables = AtomSet.empty *)

fun     convertDBtoL    (VarDB x)       presentVariables    =       let
                                                                        val newVar = fresh presentVariables
                                                                        val presentVariables = AtomSet.add(presentVariables, newVar)
                                                                        val output = Var newVar
                                                                    in
                                                                        output
                                                                    end
    |   convertDBtoL    (Lambda x)      presentVariables    =       let
                                                                        val newVar = fresh presentVariables
                                                                        val presentVariables = AtomSet.add(presentVariables, newVar)
                                                                        val restOfExp = convertDBtoL x presentVariables
                                                                        val output = Abstract(newVar, restOfExp)
                                                                    in
                                                                        output
                                                                    end             
    |   convertDBtoL    (ApplyDB (x,y)) presentVariables    =       let
                                                                        val exp1 = convertDBtoL x presentVariables
                                                                        val exp2 = convertDBtoL y presentVariables
                                                                        val output = Apply(exp1, exp2)
                                                                    in
                                                                        output
                                                                    end             
