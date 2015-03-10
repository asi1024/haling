module Typing(typing) where

import Environment
import Syntax

import Control.Monad.Identity
import Control.Monad.State

type Subst = [(Int, Ty)]

tyInt :: Ty
tyInt = TyConst "Int"

tyBool :: Ty
tyBool = TyConst "Bool"

typing :: TyEnv -> Stmt -> TyState -> (TyState, (TyEnv, Ty))
typing tyenv (Exp e) tystate = (ntystate, (tyenv, ty))
  where ((_, ty), ntystate) = runState (tyExp tyenv e) tystate
typing tyenv (Decl _) tystate = (tystate, (tyenv, Undefty))
typing tyenv (Data s l) tystate =
  if isConst s && all (\(x, y) -> isConst x && all isOK y) l
    then (tystate, (map makeTy l ++ tyenv, TyConst s))
    else error "Not a data constructor"
  where isOK x = isConst x && elem x (map fst tyenv)
        makeTy (x, y) = (x, foldr (\z -> TyFun $ TyConst z) (TyConst s) y)
typing tyenv (Import _) tystate = (tystate, (tyenv, Undefty))

tyExp :: TyEnv -> Expr -> State TyState (Subst, Ty)
tyExp _     (Val _)      = return ([], tyInt)
tyExp tyenv (Const s)    = return ([], lookupTy s tyenv)
tyExp tyenv (Var s)      = return ([], lookupTy s tyenv)
tyExp tyenv (Prim s a b) = do
  (s1, ty1) <- tyExp tyenv a
  (s2, ty2) <- tyExp tyenv b
  let (eqs3, ty) = tyPrim s ty1 ty2
  let s3 = unify $ eqsOfSubst s1 ++ eqsOfSubst s2 ++ eqs3
  return (s3, substType s3 ty)
tyExp tyenv (If a b c) = do
  (s1, ty1) <- tyExp tyenv a
  (s2, ty2) <- tyExp tyenv b
  (s3, ty3) <- tyExp tyenv c
  let s4 = unify $ eqsOfSubst s1 ++ eqsOfSubst s2 ++ eqsOfSubst s3
                ++ [(ty1, tyBool), (ty2, ty3)]
  return (s4, substType s4 ty2)
tyExp tyenv (Fix s e) = do
  ty   <- liftM TyVar freshTyvar
  (s1, ty1) <- tyExp (extendTy (s, ty) tyenv) e
  let s2 = unify $ eqsOfSubst s1 ++ [(ty, ty1)]
  return (s2, substType s2 ty)
tyExp tyenv (Fun a b) = do
  domty   <- liftM TyVar freshTyvar
  (s, ty) <- tyExp (extendTy (a, domty) tyenv) b
  return (s, TyFun (substType s domty) ty)
tyExp tyenv (App a b) = do
  (s1, ty1) <- tyExp tyenv a
  (s2, ty2) <- tyExp tyenv b
  ty        <- liftM TyVar freshTyvar
  let s3 = unify $ eqsOfSubst s1 ++ eqsOfSubst s2 ++ [(ty1, TyFun ty2 ty)]
  return (s3, substType s3 ty)

tyPrim :: String -> Ty -> Ty -> ([(Ty, Ty)], Ty)
tyPrim "+"  ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyInt)
tyPrim "-"  ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyInt)
tyPrim "*"  ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyInt)
tyPrim "==" ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyBool)
tyPrim "<"  ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyBool)
tyPrim ">"  ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyBool)
tyPrim "<=" ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyBool)
tyPrim ">=" ty1 ty2 = ([(ty1, tyInt), (ty2, tyInt)], tyBool)
tyPrim _ _ _ = ([], Undefty)

substType :: Subst -> Ty -> Ty
substType _ (TyConst a)   = (TyConst a)
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

freshTyvar :: State TyState Int
freshTyvar = StateT $ \s -> Identity (s + 1, s + 1)
