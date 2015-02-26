{-# LANGUAGE FlexibleInstances #-}
module TypingSpec where

import Test.Hspec

import Parser (parseStmt)
import Syntax
import Typing (typing)

tyStr :: String -> TyEnv -> TyState -> (TyState, (TyEnv, Ty))
tyStr str env state =
  case parseStmt str of
    Left er -> error er
    Right s -> typing env s state

typingWithEnv :: TyEnv -> TyState -> [String] -> String
typingWithEnv _ _ []     = ""
typingWithEnv e s [x]    = let (_, (_, c)) = tyStr x e s in show c
typingWithEnv e s (x:xs) = let (a, (b, _)) = tyStr x e s in typingWithEnv b a xs

tyTest :: [String] -> String
tyTest = typingWithEnv [] 0

tyPrelude :: [String] -> String
tyPrelude s = tyTest $ prelude ++ s

prelude :: [String]
prelude = ["data Bool = True | False"]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "should type basic arithmetic operations" $ do
      tyTest ["1 + 2"]   `shouldBe` "Int"
      tyTest ["-2 * 2"] `shouldBe`  "Int"
      tyTest ["1 < 2"]  `shouldBe`  "Bool"

    it "should type lambda functions" $ do
      tyTest ["\\x -> x"]   `shouldBe` "a -> a"
      tyTest ["\\x y -> x"] `shouldBe` "a -> b -> a"
      tyTest ["\\x y -> y"] `shouldBe` "a -> b -> b"
      tyTest ["(\\x -> x + 1) 2 + (\\x -> x + -1) 3"]
        `shouldBe` "Int"
      tyTest ["\\f g x -> g (f x)"]
        `shouldBe` "(a -> b) -> (b -> c) -> a -> c"
      tyTest ["\\x y z -> x z (y z)"]
        `shouldBe` "(a -> b -> c) -> (a -> b) -> a -> c"
      tyTest ["\\b x -> if x b then x else (\\x -> b)"]
        `shouldBe` "Bool -> (Bool -> Bool) -> Bool -> Bool"
      tyPrelude ["\\x -> if True then x else (if x then True else False)"]
        `shouldBe` "Bool -> Bool"
      tyTest ["\\x y -> if x then x else y"]
        `shouldBe` "Bool -> Bool -> Bool"
      tyTest ["\\n -> (\\x -> x (\\y -> y)) (\\f -> f n)"]
        `shouldBe` "a -> a"
      tyTest ["\\f x -> f x"]
        `shouldBe` "(a -> b) -> a -> b"
      tyTest ["\\x y -> x (y x)"]
        `shouldBe` "(a -> b) -> ((a -> b) -> a) -> b"
      tyTest ["\\x y -> x (y x) (y x)"]
        `shouldBe` "(a -> a -> b) -> ((a -> a -> b) -> a) -> b"
      tyTest ["\\x y z -> x (z x) (y (z x y))"]
        `shouldBe` "(((a -> b) -> a) -> b -> c) -> (a -> b)"
            ++ " -> ((((a -> b) -> a) -> b -> c) -> (a -> b) -> a) -> c"
