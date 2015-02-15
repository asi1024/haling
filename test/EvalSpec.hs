{-# LANGUAGE FlexibleInstances #-}
module EvalSpec where

import Test.Hspec

import Parser
import Declar

evalStr :: [String] -> String
evalStr [] = ""
evalStr str =
  case mapM parse str of
    Left er -> show er
    Right s -> show $ eval_ (foldl decl_ [] $ init s) $ last s
  where decl_ env st = let (r, _, _) = decl env st in r
        eval_ env st = let (_, _, e) = decl env st in e

spec :: Spec
spec = do
  describe "Eval" $ do
    it "should be return 3 with empty string" $ do
      evalStr [] `shouldBe` ""
