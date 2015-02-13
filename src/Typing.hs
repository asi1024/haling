module Typing(typing) where

import Syntax

typing :: TyEnv -> Stmt -> Check (TyEnv, Ty, Stmt)
typing tyenv st = Right (tyenv, TyInt, st)
