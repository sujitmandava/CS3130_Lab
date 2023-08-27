type var = Atom.atom

datatype    Expr    =   Var of var
                    |   Abstract of var*Expr
                    |   Apply of Expr*Expr

(*
    1. Define abstract syntax for λ-let and λ-letrec as a SML datatype.
*)

datatype    lambdaLet   =   varLet of var
                        |   absLet of var*lambdaLet
                        |   appLet of lambdaLet*lambdaLet
                        |   Let of var*lambdaLet*lambdaLet            

datatype    lambdaLetRec    =   varLetRec of var
                            |   absLetRec of var*lambdaLetRec
                            |   appLetRec of lambdaLetRec*lambdaLetRec
                            |   LetRec of var*lambdaLetRec*lambdaLetRec   


(*
    2. Write the conversion λ-let to λ-calculus.
*)

fun     convertLet (varLet x)                       =   Var x
    |   convertLet (absLet (x,expLet))              =   Abstract(x, convertLet expLet)
    |   convertLet (appLet (expLet1, expLet2))      =   Apply((convertLet expLet1), (convertLet expLet2))
    |   convertLet (Let (x, expLet1, expLet2))      =   Apply(Abstract(x,(convertLet expLet1)), (convertLet expLet2))

