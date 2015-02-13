module Eval(eval) where

import Syntax

eval :: Env -> Expr -> Expr
eval = (\_ -> id)
