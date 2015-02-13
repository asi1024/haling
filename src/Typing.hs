module Typing(typing) where

import Syntax

typing :: Stmt -> Check (Ty, Stmt)
typing st = Right (TyInt, st)
