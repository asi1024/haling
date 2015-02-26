module Typing(typing) where

import Syntax

typing :: TyEnv -> Stmt -> TyState -> (TyState, (TyEnv, Ty))
typing tyenv _ tystate = (tystate, (tyenv, TyInt))
