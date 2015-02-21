module Syntax where

type Check = Either String

err :: String -> Check a
err s = Left s

type Env = ([(String, Exval)], [(String, Exval)])
type TyEnv = [(String, Ty)]

data Expr = Val   Int
          | Const String
          | Var   String
          | Prim  String Expr Expr
          | Fun   String Expr
          | App   Expr Expr
          deriving (Eq)

data Stmt = Exp Expr
          | Decl String Expr
          | Data String [(String, [String])]
          | Import String
          deriving (Eq, Show)

data Ty = TyInt
        | TyVar Int
        | TyFun Ty Ty
        deriving (Eq)

data Exval = ValV Int
           | DatV String [String]
           | FunV Env String Expr
           | FixV Env String Expr
           deriving (Eq)

instance Show Expr where
  show (Val i)      = show i
  show (Const i)    = i
  show (Var s)      = s
  show (Prim f a b) = concat ["(Prim ", f, " ", show a, " ", show b, ")"]
  show (Fun a b)    = concat ["(\\", show a, " -> ", show b, ")"]
  show (App a b)    = concat ["(App", show a, ", ", show b, ")"]

instance Show Ty where
  show TyInt = "Int"
  show _     = undefined

instance Show Exval where
  show (ValV i) = show i
  show (DatV a l) = "(" ++ a ++ " " ++ show l ++ ")"
  show (FunV _ _ _) = "<fun>"
  show (FixV _ _ _) = "<fix>"
