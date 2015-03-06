module Syntax where

import Data.Char (isAsciiUpper)
import Data.List (union)
import Data.Maybe (fromJust)

type Check = Either String

isConst :: String -> Bool
isConst = isAsciiUpper . head

type Env = ([(String, Exval)], [(String, Exval)])
type TyEnv = [(String, Ty)]
type TyState = Int

data Expr = Val   Int
          | Const String
          | Var   String
          | Prim  String Expr Expr
          | If    Expr Expr Expr
          | Fun   String Expr
          | App   Expr Expr
          deriving (Eq)

data Stmt = Exp Expr
          | Decl [(String, Expr)]
          | Data String [(String, [String])]
          | Import String
          deriving (Eq, Show)

data Ty = TyConst String
        | TyVar Int
        | TyFun Ty Ty
        | Undefty
        deriving (Eq)

data Exval = ValV Int
           | DatV String [String]
           | FunV Env String Expr
           | FixV Env String Expr
           | Undef
           deriving (Eq)

instance Show Expr where
  show (Val i)      = show i
  show (Const i)    = i
  show (Var s)      = s
  show (Prim f a b) = concat ["(Prim ", f, " ", show a, " ", show b, ")"]
  show (If c t f)   = concat ["(If ", show c, " then ", show t, " else ", show f, ")"]
  show (Fun a b)    = concat ["(\\", show a, " -> ", show b, ")"]
  show (App a b)    = concat ["(App ", show a, ", ", show b, ")"]

instance Show Ty where
  show a = showTy (zip (freevarTy a) ['a'..]) a

freevarTy :: Ty -> [Int]
freevarTy (TyConst _) = []
freevarTy (TyVar i)   = [i]
freevarTy (TyFun a b) = freevarTy a `union` freevarTy b
freevarTy Undefty     = []

showTy :: [(Int, Char)] -> Ty -> String
showTy _ (TyConst t) = t
showTy f (TyVar i)   = [fromJust $ lookup i f]
showTy f (TyFun a@(TyFun _ _) b) = "(" ++ showTy f a ++ ") -> " ++ showTy f b
showTy f (TyFun a b) = showTy f a ++ " -> " ++ showTy f b
showTy _ Undefty     = "undef"

instance Show Exval where
  show (ValV i) = show i
  show (DatV a l) = a ++ concat (map (\x -> " " ++ x) l)
  show (FunV _ _ _) = "<fun>"
  show (FixV _ _ _) = "<fix>"
  show (Undef) = "+++"
