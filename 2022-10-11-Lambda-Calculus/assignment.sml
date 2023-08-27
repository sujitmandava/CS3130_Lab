(*
    1. Define an ML datatype expr that capture lambda calculus expressions. Use the Atom module to represent variables.
*)

type var = Atom.atom

datatype Expr = Var of var
            |   Abstract of var*Expr
            |   Apply of Expr*Expr

(*
    2. Write functions free : expr -> atom set and bound : expr -> atom set that computes free and bound variables in a 
    given lambda calculus expression respectively. You can use AtomSet structure given by the signature ORD_SET.

    free : expr -> atom set
    bound : expr -> atom set
*)

fun     all (Var x)               =   AtomSet.singleton(x)
    |   all (Abstract (x, exp))   =   
            let
                val allSet = (all exp)
                val result = AtomSet.union(allSet,AtomSet.singleton(x))
            in
                result
            end
    |   all (Apply (exp1, exp2))  =
            let
                val expSet1 = (all exp1)
                val expSet2 = (all exp2)
                val result = AtomSet.union(expSet1, expSet2)
            in
                result
            end

fun     free (Var x)               =   AtomSet.singleton(x)
    |   free (Abstract (x, exp))   =   
            let
                val expSet = (free exp)
                val result = AtomSet.subtract'(x,expSet)
            in
                result
            end
    |   free (Apply (exp1, exp2))  =
            let
                val expSet1 = (free exp1)
                val expSet2 = (free exp2)
                val result = AtomSet.union(expSet1, expSet2)
            in
                result
            end

fun bound (exp: Expr) = AtomSet.difference((all exp), (free exp))

(* 
    3. Write a function subst : expr -> atom -> expr -> expr, where
    subst e₁ x e₂ substitutes in e₁ all free occurance of x by e₂.
*)

fun     subst exp x (Var y)             = 
            if Atom.same(x,y) = true then 
                exp 
            else 
                (Var y)
    |   subst exp x (Abstract (y, e1))  = 
            if Atom.same(x,y) = true then
                Abstract (y, e1)
            else
                Abstract (y, subst exp x e1)
    |   subst exp x (Apply (e1, e2)) = Apply (subst exp x e1, subst exp x e2)


(*
    4. Write a function fresh : atom set -> atom which computes a fresh
    variable name given a set of variables.
    
    One elegant way to write the fresh function is to use Cantor's
    diagonalisation. Here is a rough sketch

        First write a function write a function
        diag : string -> string -> string which has the following property:
        z = diagonalise x y, we have
            x is a prefix of z and
            z is not a prefix of y

        Write the atom version where the second argument is atom
        of the function: write diagA : string -> atom -> string.

        Finally make use of either AtomSet.foldl or AtomSet.foldr to
        get to the fresh function by diagonalising over all elements of
        the set.
*)

fun     diagonalList [] []                  = [#"x"]
    |   diagonalList [] (x::xs)             = 
            if x=(#"x") then [#"y"] else [#"x"]
    |   diagonalList s []                   = s
    |   diagonalList (x::xs) (y::ys)        = 
            if x=y then (x::(diagonalList xs ys)) else (x::xs)

fun diag s1 s2 = String.implode(diagonalList (String.explode(s1)) (String.explode(s2)))

fun diagA (a1,a2) = Atom.atom(diag (Atom.toString(a2)) (Atom.toString(a1)))

fun fresh s = AtomSet.foldl diagA (Atom.atom("")) s