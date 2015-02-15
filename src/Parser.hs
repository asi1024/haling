{-# LANGUAGE RankNTypes #-}
module Parser(parseStmt) where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language

import Control.Monad(liftM)
import Data.Functor.Identity(Identity)
import Syntax

def :: LanguageDef st
def = emptyDef {
        P.opLetter        = oneOf "+-*=>\\"
      , P.reservedOpNames = ["+", "-", "*", "\\", "->"]
      , P.reservedNames   = ["let"]
      }

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

identifier :: Parser String
identifier = P.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

integer :: Parser Integer
integer = P.integer lexer

natural :: Parser Integer
natural = P.natural lexer


parseStmt :: String -> Check Stmt
parseStmt input = case parse stmt "" input of
                    Left  e -> Left $ show e
                    Right r -> Right r

stmt :: Parser Stmt
stmt = do
  whiteSpace
  s <- stmtBody
  eof >> return s

stmtBody :: Parser Stmt
stmtBody = liftM Exp expr <|> decl

expr :: Parser Expr
expr = try prim <|> exprNonPrim

decl :: Parser Stmt
decl = do
  symbol "let"
  name <- identifier
  reservedOp "="
  e <- expr
  return $ Decl name e

prim :: Parser Expr
prim = buildExpressionParser table exprNonPrim

table :: [[Operator String () Identity Expr]]
table = [[op_infix (reservedOp "*") (Prim "*") AssocLeft],
         [op_infix (reservedOp "+") (Prim "+") AssocLeft,
          op_infix (reservedOp "-") (Prim "-")  AssocLeft]]
    where
      op_prefix s f       = Prefix (s >> return f)
      op_infix  s f assoc = Infix (s >> return f) assoc

exprNonPrim :: Parser Expr
exprNonPrim =  try funcApp
           <|> parens expr
           <|> lambda
           <|> liftM (Val . fromIntegral) integer
           <|> liftM Var identifier

exprArg :: Parser Expr
exprArg =  parens expr
       <|> liftM (Val . fromIntegral) natural

funcApp :: Parser Expr
funcApp = do
  fun <- func
  arg <- exprArg
  floop (App fun arg)

func :: Parser Expr
func =  parens lambda
    <|> liftM Var identifier

floop :: Expr -> Parser Expr
floop fun = try ( do
                  arg <- exprArg
                  floop (App fun arg) )
         <|> return fun

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  arg <- identifier
  reservedOp "->"
  e <- expr
  return $ Fun arg e
