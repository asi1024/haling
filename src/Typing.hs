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

unify :: [(Ty, Ty)] -> Subst
unify [] = []
unify ((t1, t2) : xs) =
  if t1 == t2 || t1 == Undefty || t2 == Undefty
    then unify xs
    else case (t1, t2) of
           (TyVar x, _) -> if elem x $ freevarTy t2
                             then error "Type error."
                             else (x, t2) : (unify $ map (func [(x, t2)]) xs)
           (_, TyVar _) -> unify ((t2, t1) : xs)
           (TyFun t11 t12, TyFun t21 t22) -> unify $ (t11, t21) : (t12, t22) : xs
           (_, _) -> error "Type error."
  where func l (a, b) = (substType l a, substType l b)
