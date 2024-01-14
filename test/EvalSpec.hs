{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- EvalSpec.hs
-}

module EvalSpec (spec) where

import Test.Hspec
import Types
import Eval
import Instances()

evalASTIfCondSpec :: Spec
evalASTIfCondSpec = do
  describe "evalASTIfCond" $ do
    it "returns the true branch for a true condition" $
      evalASTIfCond (Call "if" [BoolLiteral True, IntLiteral 42, IntLiteral 99]) [] `shouldBe` Right (IntLiteral 42)

    it "returns the false branch for a false condition" $
      evalASTIfCond (Call "if" [BoolLiteral False, IntLiteral 42, IntLiteral 99]) [] `shouldBe` Right (IntLiteral 99)

    it "returns an error when condition is not a boolean" $
      evalASTIfCond (Call "if" [IntLiteral 1, IntLiteral 42, IntLiteral 99]) [] `shouldBe` Left "require three arguments"

    it "returns an error when not enough arguments are provided" $
      evalASTIfCond (Call "if" [BoolLiteral True, IntLiteral 42]) [] `shouldBe` Left "require three arguments"

evalASTSpec :: Spec
evalASTSpec = do
  describe "evalAST" $ do
    it "evaluates addition correctly" $
      evalAST (Call "+" [IntLiteral 2, IntLiteral 3]) [] `shouldBe` Right (IntLiteral 5)

    it "handles unrecognized function calls" $
      evalAST (Call "unknown" []) [] `shouldBe` Left "variable unknown not found"

spec :: Spec
spec = do
    evalASTIfCondSpec
    evalASTSpec
