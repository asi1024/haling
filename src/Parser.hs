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
      , P.reservedNames   = ["let", "import", "data", "if", "then", "else"]
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
stmtBody = liftM Exp expr <|> decl <|> imp <|> dataDef

decomposeMultArgs :: Expr -> [String] -> Expr
decomposeMultArgs = foldr Fun

decl :: Parser Stmt
decl = do
  _    <- symbol "let"
  name <- identifier
  args <- option [] (many1 identifier)
  reservedOp "="
  e <- expr
  return $ Decl name (case args of
                        []    -> e
                        args' -> decomposeMultArgs e args')

imp :: Parser Stmt
imp = do
  _ <- symbol "import"
  name <- identifier
  return $ Import name

dataDef :: Parser Stmt
dataDef = do
  _    <- symbol "data"
  name <- identifier
  reservedOp "="
  l <- many1 identifier `sepBy` (symbol "|")
  return $ Data name $ map (\x -> (head x, tail x)) l

expr :: Parser Expr
expr =  lambda
    <|> prim

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  e <- expr
  return $ decomposeMultArgs e args

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
        <|> do name <- identifier
               if isConst name
                 then return $ Const name
                 else return $ Var name
        <|> ifstmt
        <|> parens expr

ifstmt :: Parser Expr
ifstmt = do
  cond <- (symbol "if"   >> expr)
  t    <- (symbol "then" >> expr)
  f    <- (symbol "else" >> expr)
  return $ If cond t f
