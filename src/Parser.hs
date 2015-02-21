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

decl :: Parser Stmt
decl =  symbol "let" >> (try defun <|> define)

defun :: Parser Stmt
defun = do
  name <- identifier
  arg  <- identifier
  reservedOp "="
  e <- expr
  return $ Decl name (Fun arg e)

define :: Parser Stmt
define = do
  name <- identifier
  reservedOp "="
  e <- expr
  return $ Decl name e

expr :: Parser Expr
expr =  lambda
    <|> prim

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  arg <- identifier
  reservedOp "->"
  e <- expr
  return $ Fun arg e

prim :: Parser Expr
prim = buildExpressionParser table appExpr

neg :: Expr -> Expr
neg = Prim "*" (Val (-1))

table :: [[Operator String () Identity Expr]]
table = [[op_prefix (reservedOp "-") neg],
         [op_infix (reservedOp "*") (Prim "*") AssocLeft],
         [op_infix (reservedOp "+") (Prim "+") AssocLeft,
          op_infix (reservedOp "-") (Prim "-")  AssocLeft]]
    where
      op_prefix s f       = Prefix (s >> return f)
      op_infix  s f assoc = Infix (s >> return f) assoc

appExpr :: Parser Expr
appExpr = unitExpr `chainl1` (return App)

unitExpr :: Parser Expr
unitExpr =  liftM (Val . fromIntegral) natural
        <|> liftM Var identifier
        <|> parens expr
