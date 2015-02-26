module Typing(typing) where

import Syntax

import Control.Monad.State

type Subst = [(Int, Ty)]

typing :: TyEnv -> Stmt -> TyState -> (TyState, (TyEnv, Ty))
typing tyenv (Exp e) tystate = (ntystate, (tyenv, ty))
  where ((_, ty), ntystate) = runState (tyExp tyenv e) tystate
typing tyenv (Decl _ _) tystate = (tystate, (tyenv, Undefty))
typing tyenv (Data _ _) tystate = (tystate, (tyenv, Undefty))
typing tyenv (Import _) tystate = (tystate, (tyenv, Undefty))

tyExp :: TyEnv -> Expr -> State TyState (Subst, Ty)
tyExp _ _ = return ([], TyInt)

substType :: Subst -> Ty -> Ty
substType _ TyInt = TyInt
substType subst (TyVar a) =
  case subst of
    []         -> TyVar a
    ((x,y):xs) -> substType xs (if a == x then y else TyVar a)
substType subst (TyFun a b) = TyFun (substType subst a) (substType subst b)
substType _ Undefty = Undefty

eqsOfSubst :: Subst -> [(Ty, Ty)]
eqsOfSubst = map (\(a, b) -> (TyVar a, b))
