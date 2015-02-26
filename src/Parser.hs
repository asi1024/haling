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
        P.opLetter        = oneOf "+-*=><\\`"
      , P.reservedOpNames = ["+", "-", "*", "\\", "->", "<", "<=", ">", ">=", "==",
                             "`"]
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
  (name, args) <- declNames
  reservedOp "="
  e <- expr
  return $ Decl name (case args of
                        Just a  -> decomposeMultArgs e a
                        Nothing -> e)

declNames :: Parser (String, Maybe [String])
declNames =  try (do lop <- identifier
                     f   <- infixFunc
                     rop <- identifier
                     return $ (f, Just [lop, rop]))
         <|> (do name <- identifier
                 args <- optionMaybe $ many1 identifier
                 return (name, args))

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
prim = buildExpressionParser table infixAppExpr

neg :: Expr -> Expr
neg = Prim "*" (Val (-1))

table :: [[Operator String () Identity Expr]]
table = [[op_prefix (reservedOp "-") neg],
         [op_infix (reservedOp "*") (Prim "*") AssocLeft],
         [op_infix (reservedOp "+") (Prim "+") AssocLeft,
          op_infix (reservedOp "-") (Prim "-")  AssocLeft],
         [op_infix (reservedOp "<") (Prim "<") AssocLeft,
          op_infix (reservedOp "<=") (Prim "<=") AssocLeft,
          op_infix (reservedOp ">") (Prim ">") AssocLeft,
          op_infix (reservedOp ">=") (Prim ">=") AssocLeft,
          op_infix (reservedOp "==") (Prim "==") AssocLeft]]
    where
      op_prefix s f       = Prefix (s >> return f)
      op_infix  s f assoc = Infix (s >> return f) assoc

infixAppExpr :: Parser Expr
infixAppExpr = appExpr `chainl1` infixFuncApp

infixFuncApp :: Parser (Expr -> Expr -> Expr)
infixFuncApp = do
  f <- infixFunc
  return $ (\a b -> App (App (Var f) a) b)

infixFunc :: Parser String
infixFunc = between (reservedOp "`") (reservedOp "`") identifier

appExpr :: Parser Expr
appExpr = unitExpr `chainl1` (return App)

unitExpr :: Parser Expr
unitExpr =  liftM (Val . fromIntegral) natural
        <|> do name <- identifier
               if isConst name
                 then return $ Const name
                 else return $ Var name
        <|> try ifstmt
        <|> parens enclosedExpr

ifstmt :: Parser Expr
ifstmt = do
  cond <- (symbol "if"   >> expr)
  t    <- (symbol "then" >> expr)
  f    <- (symbol "else" >> expr)
  return $ If cond t f

enclosedExpr :: Parser Expr
enclosedExpr =  try expr
            <|> try incompOp
            <|> incompInfix

incompOp :: Parser Expr
incompOp = do
  lop <- optionMaybe infixAppExpr
  op  <- arithOp
  rop <- optionMaybe infixAppExpr
  return $ case (lop, rop) of
             (Just l, Nothing)  -> Fun "y" $ Prim op l (Var "y")
             (Nothing, Just r)  -> Fun "x" $ Prim op (Var "x") r
             (Nothing, Nothing) -> Fun "x" (Fun "y" $ Prim op (Var "x") (Var "y"))
             (_, _)             -> undefined

arithOp :: Parser String
arithOp = choice $ map symbol ["+", "-", "*"]

incompInfix :: Parser Expr
incompInfix =  try (do lop <- appExpr
                       f   <- infixFunc
                       return $ App (Var f) lop)
           <|> do f   <- infixFunc
                  rop <- appExpr
                  return $ Fun "x" $ App (App (Var f) (Var "x")) rop
