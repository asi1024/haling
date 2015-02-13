module Syntax where

type Check = Either String

err :: String -> Check a
err s = Left s

type Env = [(String, Expr)]
type TyEnv = [(String, Ty)]

data Expr = Val  Int
          | Var  String
          | Prim String Expr Expr
          | Fun  String Expr
          | App  Expr Expr
          deriving (Eq)

data Stmt = Exp Expr
          | Decl String Expr

data Ty = TyInt
        | TyVar Int
        | TyFun Ty Ty
        deriving (Eq)

instance Show Expr where
  show (Val i)      = show i
  show (Var s)      = s
  show (Prim f a b) = concat ["(", f, " ", show a, " ", show b, ")"]
  show (Fun _ _)    = "<fun>"
  show (App a b)    = show a ++ " " ++ show b

instance Show Ty where
  show TyInt = "int"
  show _     = undefined
