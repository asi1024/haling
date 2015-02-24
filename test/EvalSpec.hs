{-# LANGUAGE FlexibleInstances #-}
module EvalSpec where

import Test.Hspec

import Declar
import Environment
import Parser

evalStr :: [String] -> String
evalStr [] = ""
evalStr str =
  case mapM parseStmt str of
    Left er -> show er
    Right s -> show $ eval_ (foldl decl_ Environment.empty $ init s) $ last s
  where decl_ env st = let (r, _, _) = decl env st in r
        eval_ env st = let (_, _, e) = decl env st in e

spec :: Spec
spec = do
  describe "Eval" $ do
    it "should calculate basic arithmetic operations" $ do
      evalStr []           `shouldBe`     ""
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
      evalStr ["data Bool = True | False", "if True then 1 else 2"] `shouldBe` "1"
      evalStr ["data Bool = True | False",
               "(\\x -> if True then x - 1 else 2) 39"] `shouldBe` "38"
      evalStr ["data Bool = True | False",
               "let c x = False", "3 - if c 1 then 1 else 20"] `shouldBe` "-17"

    it "should apply recursive function" $ do
      evalStr ["let fact x = if x <= 0 then 1 else x * fact (x - 1)", "fact 5"] `shouldBe` "120"
      evalStr ["let sum x y = if x > y then 0 else x + sum (x + 1) y", "sum 3 5"] `shouldBe` "12"
