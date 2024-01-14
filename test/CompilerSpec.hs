--
-- EPITECH PROJECT, 2024
-- B-FUN-500-PAR-5-2-glados-hugo.mouraud
-- File description:
-- CompilerSpec
--

module CompilerSpec (spec) where

import Test.Hspec
import Types
import Compiler
import Instances()

compileSpec :: Spec
compileSpec = do
  describe "myLength" $ do
    it "returns the length of a string" $
      myLength "Hello r\n" `shouldBe` 1

  describe "convAst" $ do
    it "compiles addition AST correctly" $
      convAst (Call "+" [IntLiteral 3, IntLiteral 4]) `shouldBe` "PUSH 4\nPUSH 3\nCALL ADD\n"

    it "handles unknown function calls" $
      convAst (Call "unknown" []) `shouldBe` ""

  describe "loopAst" $ do
    it "compiles a list of ASTs correctly" $
      loopAst [Call "+" [IntLiteral 3, IntLiteral 4], IntLiteral 42] `shouldBe` "PUSH 4\nPUSH 3\nCALL ADD\nPUSH 42\n"

spec :: Spec
spec = do
    compileSpec