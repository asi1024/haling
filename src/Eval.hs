module Eval(eval) where

import Syntax

import Data.Maybe (fromJust)

prim :: Num a => String -> a -> a -> a
prim s = fromJust $ lookup s [("+", (+)), ("-", (-)), ("*", (*))]

eval :: Env -> Expr -> Exval
eval _ (Val i) = (ValV i)
eval env (Var v) =
    case fromJust $ lookup v env of
      f@(FixV fenv s e) -> eval ((s, f) : fenv) e
      exval             -> exval
eval env (Prim f a b) =
  case (eval env a, eval env b) of
    (ValV x, ValV y) -> ValV $ prim f x y
    _ -> error "Eval.eval: Prim"
eval env (Fun a b) = FunV env a b
eval env (App a b) =
  case eval env a of
    FunV fenv x y -> eval ((x, eval env b) : fenv) y
    _ -> error "Eval.eval: App"
