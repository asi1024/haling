module Declar where

import Eval(eval)
import Syntax

decl :: Env -> Ty -> Stmt -> (Env, String, Expr)
decl env _ (Exp e)    = (            env, "-"        , eval env e)
decl env t (Decl s e) = ((s, t, e) : env, "val " ++ s, eval env e)
