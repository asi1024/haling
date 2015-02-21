module Environment where

import Syntax

import Data.Maybe (fromJust)

empty :: Env
empty = ([], [])

extendVar :: (String, Exval) -> Env -> Env
extendVar x (a, b) = (a, x : b)

lookupVar :: String -> Env -> Exval
lookupVar x (_, b) = fromJust $ lookup x b
