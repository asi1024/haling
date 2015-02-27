module Environment where

import Syntax

import Data.Maybe (fromJust)

empty :: Env
empty = ([], [])

extendVar :: (String, Exval) -> Env -> Env
extendVar x (a, b) = (a, x : b)

lookupVar :: String -> Env -> Exval
lookupVar x (_, b) = fromJust $ lookup x b

existVar :: String -> Env -> Bool
existVar x (_, b) =
    case lookup x b of Just _  -> True
                       Nothing -> False


onlyConst :: Env -> [(String, [String])] -> Bool
onlyConst (a, _) l = all (not . (`elem` hasDefined)) newDefined
    where hasDefined = map fst a
          newDefined = map fst l

extendConst :: (String, [String]) -> Env -> Env
extendConst (x, []) (a, b) = ((x, DatV x []) : a, b)
extendConst _ _ = undefined

lookupConst :: String -> Env -> Exval
lookupConst x (a, _) = fromJust $ lookup x a

lookupTy :: String -> TyEnv -> Ty
lookupTy s tyenv =
  case lookup s tyenv of
    Nothing -> error $ "Not in scope: \"" ++ s ++ "\""
    Just a  -> a

extendTy :: (String, Ty) -> TyEnv -> TyEnv
extendTy x xs = (x : xs)
