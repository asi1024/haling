{-# LANGUAGE FlexibleInstances #-}
module EvalSpec where

import Test.Hspec

import Parser
import Declar

evalStr :: [String] -> String
evalStr [] = ""
evalStr str =
  case mapM parseStmt str of
    Left er -> show er
    Right s -> show $ eval_ (foldl decl_ [] $ init s) $ last s
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

    it "should return lambda expression" $ do
      evalStr ["\\x -> x"] `shouldBe` "<fun>"
      evalStr ["\\x -> x + 1"] `shouldBe` "<fun>"
      evalStr ["\\y -> 99 + (\\x -> x + y)"] `shouldBe` "<fun>"

    it "should apply function" $ do
      evalStr ["(\\x -> x) 22"] `shouldBe` "22"
      evalStr ["(\\x -> x + 33) 22"] `shouldBe` "55"
      evalStr ["(\\x -> (\\y -> x + y)) 19 32"] `shouldBe` "51"
      evalStr ["(\\f -> (\\x -> f x + 23)) (\\x -> x + 7) 100"] `shouldBe` "130"

    it "should operate declaration" $ do
      evalStr ["let a = 10", "a + 22"] `shouldBe` "32"
      evalStr ["let i = (\\i -> i)", "(i i) (i 10)"] `shouldBe` "10"
