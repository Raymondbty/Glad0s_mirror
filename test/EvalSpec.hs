{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- EvalSpec.hs
-}

module EvalSpec (spec) where

import Test.Hspec
import Ast
import Eval
import Instances()

evalASTIfCondSpec :: Spec
evalASTIfCondSpec = do
  describe "evalASTIfCond" $ do
    it "returns 12 for (if (eq? 5 5) (+ 6 6) (- 7 1))" $
      evalASTIfCond (Call "if" [Call "eq?" [IntLiteral 5, IntLiteral 5], Call "+" [IntLiteral 6, IntLiteral 6], Call "-" [IntLiteral 7, IntLiteral 1]]) `shouldBe` Right (IntLiteral 12)

    it "returns 6 for (if (eq? 5 4) (+ 6 6) (- 7 1))" $
      evalASTIfCond (Call "if" [Call "eq?" [IntLiteral 5, IntLiteral 4], Call "+" [IntLiteral 6, IntLiteral 6], Call "-" [IntLiteral 7, IntLiteral 1]]) `shouldBe` Right (IntLiteral 6)

    it "returns 12 for (if (eq? \"test\" \"test\") (+ 6 6) (- 7 1))" $
      evalASTIfCond (Call "if" [Call "eq?" [StringLiteral "test", StringLiteral "test"], Call "+" [IntLiteral 6, IntLiteral 6], Call "-" [IntLiteral 7, IntLiteral 1]]) `shouldBe` Right (IntLiteral 12)

    it "returns 12 for (if (eq? \"test\" \"test\") (+ 6 6) (- 7 1))" $
      evalASTIfCond (Call "if" [Call "eq?" [StringLiteral "test", StringLiteral "test"], Call "+" [IntLiteral 6, IntLiteral 6], Call "-" [IntLiteral 7, IntLiteral 1]]) `shouldBe` Right (IntLiteral 12)

evalASTSpec :: Spec
evalASTSpec = do
  describe "evalAST" $ do
    it "evaluates addition correctly" $
      evalAST (Call "+" [IntLiteral 2, IntLiteral 3]) `shouldBe` Right (IntLiteral 5)

    it "handles unrecognized function calls" $
      evalAST (Call "unknown" []) `shouldBe` Left "no matching function 'unknown' (unknown)"

spec :: Spec
spec = do
    evalASTIfCondSpec
    evalASTSpec
