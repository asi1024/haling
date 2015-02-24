module Eval(eval) where

import Environment
import Syntax

import Data.Maybe (fromJust)

prim :: String -> Int -> Int -> Int
prim s = fromJust $ lookup s [("+", (+)), ("-", (-)), ("*", (*))]

comp :: String -> Int -> Int -> Exval
comp s = fromJust $ lookup s [("==", eq), ("<", lt), ("<=", le), (">", gt), (">=", ge)]

primTrue :: Exval
primTrue = DatV "True" []

primFalse :: Exval
primFalse = DatV "False" []

eq :: Int -> Int -> Exval
eq a b = if a == b then primTrue else primFalse

lt :: Int -> Int -> Exval
lt a b = if a < b then primTrue else primFalse

le :: Int -> Int -> Exval
le a b = if a <= b then primTrue else primFalse

gt :: Int -> Int -> Exval
gt a b = if a > b then primTrue else primFalse

ge :: Int -> Int -> Exval
ge a b = if a >= b then primTrue else primFalse

eval :: Env -> Expr -> Exval
eval _ (Val i)   = (ValV i)
eval env (Const c) =
    case lookupConst c env of
      f@(FixV fenv s e) -> eval (extendVar (s, f) fenv) e
      exval             -> exval
eval env (Var v) =
    case lookupVar v env of
      f@(FixV fenv s e) -> eval (extendVar (s, f) fenv) e
      exval             -> exval
eval env (Prim f a b) =
  case (eval env a, eval env b) of
    (ValV x, ValV y) -> ValV $ prim f x y
    _ -> error "Eval.eval: Prim"
eval env (Comp f a b) =
    case (eval env a, eval env b) of
      (ValV x, ValV y) -> comp f x y
      _ -> error "Eval.eval: Prim"
eval env (If c t f) =
    case eval env c of
      (DatV "True"  []) -> eval env t
      (DatV "False" []) -> eval env f
      _                 -> error "Eval.eval: If"
eval env (Fun a b) = FunV env a b
eval env (App a b) =
  case eval env a of
    FunV fenv x y -> eval (extendVar (x, eval env b) fenv) y
    _ -> error "Eval.eval: App"
