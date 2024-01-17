{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- GladosSpec.hs
-}

module GladosSpec (spec) where

import Test.Hspec
import System.Process()
import Glados
import Types
import Instances()

getSymbolSpec :: Spec
getSymbolSpec = do
  describe "getSymbol" $ do
    it "returns the symbol from SSymbol" $
      getSymbol (SSymbol "example") `shouldBe` Just "example"

    it "returns Nothing for SInt" $
      getSymbol (SInt 42) `shouldBe` Nothing

    it "returns Nothing for SList" $
      getSymbol (SList []) `shouldBe` Nothing

getIntegerSpec :: Spec
getIntegerSpec = do
  describe "getInteger" $ do
    it "returns the integer from SInt" $
      getInteger (SInt 42) `shouldBe` Just 42

    it "returns Nothing for SSymbol" $
      getInteger (SSymbol "example") `shouldBe` Nothing

    it "returns Nothing for SList" $
      getInteger (SList []) `shouldBe` Nothing

getListSpec :: Spec
getListSpec = do
  describe "getList" $ do
    it "returns the list from SList" $
      getList (SList [SSymbol "a", SSymbol "b"]) `shouldBe` Just [SSymbol "a", SSymbol "b"]

    it "returns Nothing for SSymbol" $
      getList (SSymbol "example") `shouldBe` Nothing

    it "returns Nothing for SInt" $
      getList (SInt 42) `shouldBe` Nothing

printTreeSpec :: Spec
printTreeSpec = do
  describe "printTree" $ do
    it "prints SInt correctly" $
      printTree (SInt 42) `shouldBe` Just "a Number 42"

    it "prints SSymbol correctly" $
      printTree (SSymbol "example") `shouldBe` Just "a Symbol 'example'"

    it "prints an empty SList correctly" $
      printTree (SList []) `shouldBe` Just "an empty List"

    it "prints SList with elements correctly" $
      printTree (SList [SSymbol "a", SSymbol "b"]) `shouldBe` Just "(a List with a Symbol 'a' followed by a Symbol 'b')"

    it "prints SList with nothing followed by elements correctly" $
      printTree (SList []) `shouldBe` Just "an empty List"

printTreeListSpec :: Spec
printTreeListSpec = do
  describe "printTreeList" $ do
    it "prints an empty list correctly" $
      printTreeList [] `shouldBe` "nothing"

    it "prints a list with one element correctly" $
      printTreeList [SInt 42] `shouldBe` "a Number 42"

    it "prints a list with multiple elements correctly" $
      printTreeList [SInt 1, SSymbol "example", SList [SSymbol "a", SSymbol "b"]] `shouldBe`
        "a Number 1, a Symbol 'example', (a List with a Symbol 'a' followed by a Symbol 'b')"

    it "handles empty string when an element cannot be printed" $
      printTreeList [SInt 1, SSymbol "example", SList [SSymbol "a", SSymbol "b", SList []]] `shouldBe`
        "a Number 1, a Symbol 'example', (a List with a Symbol 'a' followed by a Symbol 'b', an empty List)"

    it "handles Nothing case when printing a list with elements" $
      printTreeList [SSymbol "a", SSymbol "b", SList []] `shouldBe` "a Symbol 'a', a Symbol 'b', an empty List"

    it "handles Nothing case when printing an empty list" $
      printTreeList [] `shouldBe` "nothing"

sexprToASTSpec :: Spec
sexprToASTSpec = do
  describe "sexprToAST" $ do
    it "converts SInt to Ast correctly" $
      sexprToAST (SInt 42) `shouldBe` Just (IntLiteral 42)

    it "converts SSymbol to Ast correctly" $
      sexprToAST (SSymbol "example") `shouldBe` Just (StringLiteral "example")

    it "converts SList to Ast correctly" $
      sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 42]) `shouldBe` Just (Define "x" (IntLiteral 42))

spec :: Spec
spec = do
    getSymbolSpec
    getIntegerSpec
    getListSpec
    printTreeSpec
    printTreeListSpec
    sexprToASTSpec
