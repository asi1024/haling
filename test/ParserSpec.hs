{-# LANGUAGE FlexibleInstances #-}
module ParserSpec where

import Test.Hspec

import Parser
import Syntax

spec :: Spec
spec = do
  describe "Parser" $ do
    it "should return Val 1 with 1" $ do
      parseStmt "1" `shouldBe` (Right $ Exp $ Val 1)

    it "should return (-1) * a with -a" $ do
      parseStmt "-a" `shouldBe` (Right $ Exp $ Prim "*" (Val (-1)) (Var "a"))

    it "should return (- a 1) with a - 1" $ do
      parseStmt "a - 1" `shouldBe` (Right $ Exp $ Prim "-" (Var "a") (Val 1))

    it "should return App" $ do
      parseStmt "map (\\x->x) a"
                `shouldBe` (Right $ Exp $ App (App (Var "map") (Fun "x" (Var "x"))) (Var "a"))

    it "should return Decl with let" $ do
      parseStmt "let x = a" `shouldBe` (Right $ Decl "x" (Var "a"))
