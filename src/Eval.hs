module Eval(eval) where

import Environment
import Syntax

import Data.Maybe (fromJust)

prim :: String -> Exval -> Exval -> Exval
prim s = fromJust $ lookup s [("+", plus), ("-", sub), ("*", mult),
                              ("==", eq), ("<", lt), ("<=", le), (">", gt), (">=", ge)]

plus :: Exval -> Exval -> Exval
plus (ValV a) (ValV b) = ValV $ a + b
plus _ _ = error "Eval.Eval.Prim: plus"

sub :: Exval -> Exval -> Exval
sub (ValV a) (ValV b) = ValV $ a - b
sub _ _ = error "Eval.Eval.Prim: sub"

mult :: Exval -> Exval -> Exval
mult (ValV a) (ValV b) = ValV $ a * b
mult _ _ = error "Eval.Eval.Prim: mult"

primTrue :: Exval
primTrue = DatV "True" []

primFalse :: Exval
primFalse = DatV "False" []

eq :: Exval -> Exval -> Exval
eq (ValV a) (ValV b) = if a == b then primTrue else primFalse
eq _ _ = error "Eval.Eval.Prim: eq"

lt :: Exval -> Exval -> Exval
lt (ValV a) (ValV b) = if a < b then primTrue else primFalse
lt _ _ = error "Eval.Eval.Prim: lt"

le :: Exval -> Exval -> Exval
le (ValV a) (ValV b) = if a <= b then primTrue else primFalse
le _ _ = error "Eval.Eval.Prim: le"

gt :: Exval -> Exval -> Exval
gt (ValV a) (ValV b) = if a > b then primTrue else primFalse
gt _ _ = error "Eval.Eval.Prim: gt"

ge :: Exval -> Exval -> Exval
ge (ValV a) (ValV b) = if a >= b then primTrue else primFalse
ge _ _ = error "Eval.Eval.Prim: ge"

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
eval env (Prim f a b) = prim f (eval env a) (eval env b)
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
