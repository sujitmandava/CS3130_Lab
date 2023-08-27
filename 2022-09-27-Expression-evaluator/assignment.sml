(*
    1. Design a small programming language which consists of two things
        An assignement of variables x := e
        Printing an expresions value print e
*)

datatype Expr = Const of real
            |   Var of string
            |   Plus of Expr*Expr
            |   Mul of Expr*Expr

datatype Stmt = Assign of string*Expr
            |   Print of Expr

type Program = Stmt list

(* 
    2. Design an interpreter for the language by defining two functions.

    val eval      : Env -> Expr -> real option
        (* Evaluates an expression. The result is real option to take care of
        cases when there are undefined variables *)
    val execute   : Env -> Stmt -> Env
        (* Executes a single statement and returns the modified environment *)
    val interpret : Program -> unit
        (* Run the program starting with an empty environment
        This is essentially a fold from the left.*) 
*)

type Env = real AtomMap.map

fun     eval (env: Env) (Const x)      = SOME x
    |   eval (env: Env) (Var x)        = AtomMap.find(env, Atom.atom x)
    |   eval (env: Env) (Plus (x,y))   = 
            let
                val result = case (eval env x) of
                   NONE => NONE
                 | SOME xreal => case (eval env y) of
                    NONE => NONE
                  | SOME yreal => SOME (xreal+yreal)
            in
                result
            end
    |   eval (env: Env) (Mul (x,y))    = 
            let
                val result = case (eval env x) of
                   NONE => NONE
                 | SOME xreal => case (eval env y) of
                    NONE => NONE
                  | SOME yreal => SOME (xreal*yreal)
            in
                result
            end

fun     exec (env: Env) (Print x)         = 
            let
              val result = case (eval env x) of
                                NONE        => ((print "Not present in environment\n"); env)
                            |   (SOME xres) => ((print (Real.toString xres)); env)
            in
              result
            end
    |   exec (env: Env) (Assign(x,y))     =
            let
              val result = case (eval env y) of
                                NONE        => env
                            |   SOME yres   => AtomMap.insert(env, Atom.atom x, yres)
            in
              result
            end