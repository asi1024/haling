module Declar where

import Eval(eval)
import Syntax

decl :: Env -> Stmt -> (Env, String, Exval)
decl env (Exp e)    = ( env, "-"        , eval env e)
decl env (Decl s e) = (nenv, "val " ++ s, fix_e)
  where fix_e = FixV env s e
        nenv  = (s, fix_e) : env
decl _ (Data _ _) = undefined
decl _ (Import _) = undefined
