module Parser(parse) where

import Syntax

parse :: String -> Check Stmt
parse _ = Right $ Exp $ Var "w"
