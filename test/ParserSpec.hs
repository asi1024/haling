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

    it "should return wrapped primitive operator" $ do
      opExpr "+"  `shouldBe` Fun "@x" (Fun "@y" $ Prim "+" (Var "@x") (Var "@y"))
      opExpr "&=" `shouldBe` Fun "@x" (Fun "@y" $ App (App (Var "&=") (Var "@x")) (Var "@y"))
      opApp "+" (Val 1) (Var "a") `shouldBe` App (App (opExpr "+") (Val 1)) (Var "a")

    it "should return (-1) * a with -a" $ do
      parseStmt "-a" `shouldBe` (Right $ Exp $ opApp "*" (Val $ -1) (Var "a"))

    it "should return Prim" $ do
      parseStmt "a - 1 + 3" `shouldBe` (Right $ Exp $ opApp "+" (opApp "-" (Var "a") (Val 1)) (Val 3))
      parseStmt "1 - 3 * a" `shouldBe` (Right $ Exp $ opApp "-" (Val 1) (opApp "*" (Val 3) (Var "a")))

    it "should return Comp" $ do
      parseStmt "1 < 2"           `shouldBe` (Right $ Exp $ opApp "<" (Val 1) (Val 2))
      parseStmt "1 <= 3 + a"      `shouldBe` (Right $ Exp $ opApp "<=" (Val 1) (opApp "+" (Val 3) (Var "a")))
      parseStmt "\\x y -> x == y" `shouldBe` (Right $ Exp $ Fun "x" $ Fun "y" $ opApp "==" (Var "x") (Var "y"))

    it "should return Fun with lambda expr" $ do
      parseStmt "\\x -> x" `shouldBe` (Right $ Exp $ Fun "x" (Var "x"))
      parseStmt "\\x y -> x + y"
                    `shouldBe` (Right $ Exp $ Fun "x" (Fun "y" $ opApp "+" (Var "x") (Var "y")))

    it "should return App" $ do
      parseStmt "map (\\x->x) a"
                `shouldBe` (Right $ Exp $ App (App (Var "map") (Fun "x" (Var "x"))) (Var "a"))

    it "should return Decl with let" $ do
      parseStmt "let x = a" `shouldBe` (Right $ Decl [("x", Var "a")])

    it "should return Decl with let" $ do
      parseStmt "let f x = x + 1"
                    `shouldBe` (Right $ Decl [("f", (Fun "x" (opApp "+" (Var "x") (Val 1))))])
      parseStmt "let f x y = x + y"
                    `shouldBe` (Right $ Decl [("f", (Fun "x" (Fun "y" $ opApp "+" (Var "x") (Var "y"))))])

    it "should return If with if-stmt" $ do
      parseStmt "if True then 1 + 1 else 2"
                    `shouldBe` (Right $ Exp $ If (Const "True") (opApp "+" (Val 1) (Val 1)) (Val 2))
      parseStmt "\\x -> x - if True then 1 else 2"
                    `shouldBe` (Right $ Exp $ (Fun "x" (opApp "-" (Var "x") (If (Const "True") (Val 1) (Val 2)))))
      parseStmt "if x > 10 then 1 else 0"
                    `shouldBe` (Right $ Exp $ If (opApp ">" (Var "x") (Val 10)) (Val 1) (Val 0))

    it "should parse infix function" $ do
      parseStmt "1 * 1 `f` 1 - 1"
                    `shouldBe` (Right $ Exp $ opApp "-" (opApp "*" (Val 1) (App (App (Var "f") (Val 1)) (Val 1))) (Val 1))
      parseStmt "l `f` g r"
                    `shouldBe` (Right $ Exp $ App (App (Var "f") (Var "l")) (App (Var "g") (Var "r")))
      parseStmt "if f then t else 1 `f` a"
                    `shouldBe` (Right $ Exp $ If (Var "f") (Var "t") (App (App (Var "f") (Val 1)) (Var "a")))
      parseStmt "let x `f` y = 1"
                    `shouldBe` (Right $ Decl [("f", (Fun "x" (Fun "y" (Val 1))))])
      parseStmt "let (&=) = (+)"
                    `shouldBe` (Right $ Decl [("&=", opExpr "+")])
      parseStmt "let a &= b = a + b"
                    `shouldBe` (Right $ Decl [("&=", Fun "a" (Fun "b" $ opApp "+" (Var "a") (Var "b")))])

    it "should parse partial applying of infix operator and function" $ do
      parseStmt "(+)" `shouldBe` (Right $ Exp $ opExpr "+")
      parseStmt "(+ f a)" `shouldBe` (Right $ Exp $ Fun "@x" $ opApp "+" (Var "@x") (App (Var "f") (Var "a")))
      parseStmt "(1 `f` a -)" `shouldBe` (Right $ Exp $ App (opExpr "-") $ App (App (Var "f") (Val 1)) (Var "a"))

    it "AST of syntactic sugar must be equal to one of the original syntax" $ do
      parseStmt "f 1 a"   `shouldBe` parseStmt "1 `f` a"
      parseStmt "f 1"     `shouldBe` parseStmt "(1 `f`)"
      parseStmt "(1 +) 1" `shouldBe` parseStmt "1 + 1"
      parseStmt "let x `f` y = x + y" `shouldBe` parseStmt "let f x y = x + y"

    it "should parse multiple binds" $ do
      parseStmt "let x = 1; x &= y = x + y" `shouldBe` (Right $ Decl $ [("x", (Val 1)),
                                                                        ("&=", Fun "x" (Fun "y" $ opApp "+" (Var "x") (Var "y")))])
      parseStmt "let f x = x * 2; a = f 2" `shouldBe` (Right $ Decl $ [("f", Fun "x" $ opApp "*" (Var "x") (Val 2)),
                                                                       ("a", App (Var "f") (Val 2))])

    it "should parse local definition" $ do
      parseStmt "let a = 1 in a" `shouldBe` (Right $ Exp $ App (Fun "a" (Var "a")) (Fix "a" (Val 1)))
      parseStmt "let a = \\x -> x; b = 2; in a b"
                    `shouldBe` (Right $ Exp $ App (Fun "a" (App (Fun "b" (App (Var "a") (Var "b"))) (Fix "b" (Val 2)))) (Fix "a" (Fun "x" (Var "x"))))
      parseStmt "let a = 1 in let b = 2 in a + b"
                    `shouldBe` (Right $ Exp $ App (Fun "a" (App (Fun "b" (opApp "+" (Var "a") (Var "b"))) (Fix "b" (Val 2)))) (Fix "a" (Val 1)))
      parseStmt "1 + let a = 1 in a"
                    `shouldBe` (Right $ Exp $ opApp "+" (Val 1) (App (Fun "a" (Var "a")) (Fix "a" (Val 1))))
