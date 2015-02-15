{-# LANGUAGE FlexibleInstances #-}
module ParserSpec where

import Test.Hspec

import Parser
import Syntax

spec :: Spec
spec = do
  describe "Parser" $ do
    it "should be return Var w with empty string" $ do
      parse "" `shouldBe` (Right $ Exp $ Var "w")
