module Declar where

import Environment
import Eval(eval)
import Syntax

decl :: Env -> Stmt -> (Env, String, Exval)
decl env (Exp e)    = ( env, "-", eval env e)
decl env (Decl s e) = (nenv,   s, fix_e)
  where fix_e = FixV env s e
        nenv  = extendVar (s, fix_e) env
decl env (Data _ l) = (foldr extendConst env l, "-", Undef)
decl _   (Import _) = undefined
