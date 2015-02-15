module Eval(eval) where

import Syntax

import Data.Maybe (fromJust)

prim :: Num a => String -> a -> a -> a
prim s = fromJust $ lookup s [("+", (+)), ("-", (-)), ("*", (*))]

eval :: Env -> Expr -> Expr
eval _ (Val i) = (Val i)
eval env (Var v) = fromJust $ lookup v env
eval env (Prim f a b) =
  case (eval env a, eval env b) of
    (Val x, Val y) -> Val $ prim f x y
    _ -> error "Eval.eval: Prim"
eval _ (Fun a b) = Fun a b
eval env (App a b) =
  case eval env a of
    Fun x y -> eval ((x, eval env b) : env) y
    _ -> error "Eval.eval: App"
