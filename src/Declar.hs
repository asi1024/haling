module Declar where

import Eval(eval)
import Syntax

decl :: Env -> Stmt -> (Env, String, Exval)
decl env (Exp e)    = ( env, "-"        , eval env e)
decl env (Decl s e) = (nenv, "val " ++ s, eval env e)
  where nenv = (s, eval env e) : env
