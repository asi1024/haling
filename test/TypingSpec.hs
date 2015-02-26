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

spec :: Spec
spec = do
  describe "Eval" $ do
    it "should type basic arithmetic operations" $ do
      tyTest ["1 + 2"]    `shouldBe`  "Int"
      tyTest ["-2 * 2"]   `shouldBe`  "Int"

    it "should type lambda functions" $ do
      tyTest ["\\x -> x"]   `shouldBe` "a -> a"
      tyTest ["\\x y -> x"] `shouldBe` "a -> b -> a"
      tyTest ["\\x y -> y"] `shouldBe` "a -> b -> b"
      tyTest ["(\\x -> x + 1) 2 + (\\x -> x + -1) 3"] `shouldBe` "Int"
      tyTest ["\\f g x -> g (f x)"] `shouldBe` "(a -> b) -> (b -> c) -> a -> c"
      tyTest ["\\x y z -> x z (y z)"] `shouldBe` "(a -> b -> c) -> (a -> b) -> a -> c"
      tyTest ["\\f g x -> g (f x)"] `shouldBe` "(a -> b) -> (b -> c) -> a -> c"
      tyTest ["\\f g x -> g (f x)"] `shouldBe` "(a -> b) -> (b -> c) -> a -> c"
      tyTest ["\\f x -> f x"] `shouldBe` "(a -> b) -> a -> b"
