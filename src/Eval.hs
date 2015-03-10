module Eval(eval) where

import Environment
import Syntax

primBool :: Bool -> Exval
primBool True  = DatV "True" []
primBool False = DatV "False" []

prim :: String -> Exval -> Exval -> Exval
prim "+"  (ValV a) (ValV b) = ValV $ a + b
prim "-"  (ValV a) (ValV b) = ValV $ a - b
prim "*"  (ValV a) (ValV b) = ValV $ a * b
prim "==" (ValV a) (ValV b) = primBool $ a == b
prim "<"  (ValV a) (ValV b) = primBool $ a <  b
prim "<=" (ValV a) (ValV b) = primBool $ a <= b
prim ">"  (ValV a) (ValV b) = primBool $ a >  b
prim ">=" (ValV a) (ValV b) = primBool $ a >= b
prim _ _ _ = error "Eval.Eval.Prim"

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
eval env (Fix s e) = FixV env s e
eval env (Fun a b) = FunV env a b
eval env (App a b) =
  case eval env a of
    FunV fenv x y -> eval (extendVar (x, eval env b) fenv) y
    _ -> error "Eval.eval: App"
