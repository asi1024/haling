{-# LANGUAGE FlexibleInstances #-}
module EvalSpec where

import Test.Hspec

import Declar (decl)
import Environment (empty)
import Parser (parseStmt)
import Syntax (Env, Exval)

declStr :: String -> Env -> (Env, String, Exval)
declStr str env =
  case parseStmt str of
    Left er -> error er
    Right s -> decl env s

decl_ :: Env -> String -> Env
decl_ env str = let (e, _, _) = declStr str env in e

eval_ :: Env -> String -> String
eval_ env str = let (_, _, e) = declStr str env in show e

evalStrWithEnv :: Env -> [String] -> String
evalStrWithEnv env str = eval_ (foldl decl_ env $ init str) $ last str

evalStr :: [String] -> String
evalStr = evalStrWithEnv empty

evalPrelude :: [String] -> String
evalPrelude = evalStrWithEnv $ foldl decl_ empty prelude

prelude :: [String]
prelude = ["data Bool = True | False"]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "should calculate basic arithmetic operations" $ do
      evalStr ["23"]       `shouldBe`   "23"
      evalStr ["-12"]      `shouldBe`  "-12"
      evalStr ["13-16"]    `shouldBe`   "-3"
      evalStr ["22+13-16"] `shouldBe`   "19"
      evalStr ["22-13+16"] `shouldBe`   "25"
      evalStr ["22*13+16"] `shouldBe`  "302"
      evalStr ["22+13*16"] `shouldBe`  "230"
      evalStr ["22*13-16"] `shouldBe`  "270"
      evalStr ["22-13*16"] `shouldBe` "-186"

    it "should calculate basic conparison operations" $ do
      evalStr ["1 <= 3"]       `shouldBe` "True"
      evalStr ["1 + 4 < 3"]    `shouldBe` "False"
      evalStr ["1 > 3 - 10"]   `shouldBe` "True"
      evalStr ["1 >= -4 * 10"] `shouldBe` "True"
      evalStr ["1 == -4 * 10"] `shouldBe` "False"
      evalStr ["1 == 1"]       `shouldBe` "True"

    it "should return lambda expression" $ do
      evalStr ["\\x -> x"] `shouldBe` "<fun>"
      evalStr ["\\x -> x + 1"] `shouldBe` "<fun>"
      evalStr ["\\y -> 99 + (\\x -> x + y)"] `shouldBe` "<fun>"

    it "should apply function" $ do
      evalStr ["(\\x -> x) 22"] `shouldBe` "22"
      evalStr ["(\\x -> x + 33) 22"] `shouldBe` "55"
      evalStr ["(\\x -> (\\y -> x + y)) 19 32"] `shouldBe` "51"
      evalStr ["(\\f -> (\\x -> f x + 23)) (\\x -> x + 7) 100"] `shouldBe` "130"
      evalStr ["(\\x y -> x - y) 19 32"] `shouldBe` "-13"

    it "should operate declaration" $ do
      evalStr ["let a = 10", "a + 22"] `shouldBe` "32"
      evalStr ["let i = (\\i -> i)", "(i i) (i 10)"] `shouldBe` "10"
      evalStr ["let f x   = x + 1", "f 1"] `shouldBe` "2"
      evalStr ["let f x y = x * y", "let g = f 2", "g 3 + g 4"] `shouldBe` "14"

    it "should operate if" $ do
      evalPrelude ["if True then 1 else 2"] `shouldBe` "1"
      evalPrelude ["(\\x -> if True then x - 1 else 2) 39"] `shouldBe` "38"
      evalPrelude ["let c x = False", "3 - if c 1 then 1 else 20"] `shouldBe` "-17"

    it "should apply recursive function" $ do
      evalStr ["let fact x = if x <= 0 then 1 else x * fact (x - 1)", "fact 5"] `shouldBe` "120"
      evalStr ["let sum x y = if x > y then 0 else x + sum (x + 1) y", "sum 3 5"] `shouldBe` "12"

    it "should caluculate infix function" $ do
      evalStr ["let f x y = x - y", "f 10 5 `f` 3"] `shouldBe` "2"
      evalStr ["let f x y = x - y", "10 `f` 5 `f` 3"] `shouldBe` "2"
      evalStr ["let f x y = x - y", "(1 `f`) 4"] `shouldBe` "-3"
      evalStr ["let f x y = x - y", "let g = (`f` 2)", "g 4"] `shouldBe` "2"
      evalStr ["let f = (-)", "f 1 3"] `shouldBe` "-2"
      evalStr ["(+ 1) 3"] `shouldBe` "4"
      evalStr ["(4 -) 3"] `shouldBe` "1"
      evalStr ["let x `f` y = x - y", "10 `f` 5 `f` 3"] `shouldBe` "2"
