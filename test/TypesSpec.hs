{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstSpec.hs
-}

module TypesSpec (spec) where

import Test.Hspec
import Types
import Instances()

astSpec :: Spec
astSpec = do
  describe "Ast" $ do
    it "shows Define correctly" $
      show (Define "x" (IntLiteral 42)) `shouldBe` "Define \"x\" (IntLiteral 42)"

    it "shows Call correctly" $
      show (Call "add" [IntLiteral 1, IntLiteral 2]) `shouldBe` "Call \"add\" [IntLiteral 1,IntLiteral 2]"

    it "shows IntLiteral correctly" $
      show (IntLiteral 42) `shouldBe` "IntLiteral 42"

    it "shows StringLiteral correctly" $
      show (StringLiteral "hello") `shouldBe` "StringLiteral \"hello\""

    it "shows Symbol correctly" $
      show (Symbol "x") `shouldBe` "Symbol \"x\""

    it "shows BoolLiteral correctly" $
      show (BoolLiteral True) `shouldBe` "BoolLiteral True"

envSpec :: Spec
envSpec = do
  describe "Env" $ do
    it "shows Var correctly" $
      show (Var "x" (IntLiteral 42)) `shouldBe` "Var \"x\" (IntLiteral 42)"

    it "shows Var with Call correctly" $
      show (Var "y" (Call "add" [IntLiteral 1, IntLiteral 2])) `shouldBe`
        "Var \"y\" (Call \"add\" [IntLiteral 1,IntLiteral 2])"

    it "shows Var with StringLiteral correctly" $
      show (Var "z" (StringLiteral "hello")) `shouldBe`
        "Var \"z\" (StringLiteral \"hello\")"

spec :: Spec
spec = do
    astSpec
    envSpec
