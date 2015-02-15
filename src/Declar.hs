module Declar where

import Eval(eval)
import Syntax

decl :: Env -> Stmt -> (Env, String, Expr)
decl env (Exp e)    = (         env, "-"        , eval env e)
decl env (Decl s e) = ((s, e) : env, "val " ++ s, eval env e)
