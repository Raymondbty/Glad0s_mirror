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

evalASTSpec :: Spec
evalASTSpec = do
  describe "evalAST" $ do
    it "evaluates addition correctly" $
      evalAST (Call "+" [IntLiteral 2, IntLiteral 3]) `shouldBe` Right (IntLiteral 5)

    it "handles unrecognized function calls" $
      evalAST (Call "unknown" []) `shouldBe` Left "no matching function 'unknown' (unknown)"

spec :: Spec
spec = do
    evalASTSpec
