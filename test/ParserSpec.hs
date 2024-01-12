{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- ParserSpec.hs
-}

module ParserSpec (spec) where

import Test.Hspec
import Types
import Parser
import Glados
import Instances()

parseCharSpec :: Spec
parseCharSpec = do
  describe "parseChar" $ do
    it "parses a matching character correctly" $
      parseChar 'a' "abc" `shouldBe` Just ('a', "bc")

    it "fails to parse a non-matching character" $
      parseChar 'a' "def" `shouldBe` Nothing

    it "fails to parse an empty string" $
      parseChar 'a' "" `shouldBe` Nothing

    it "handles parsing multiple matching characters" $
      parseChar 'a' "aaabbb" `shouldBe` Just ('a', "aabbb")

    it "handles parsing multiple matching characters at the beginning of a string" $
      parseChar 'a' "aaaabbb" `shouldBe` Just ('a', "aaabbb")

    it "handles parsing multiple matching characters with repetition" $
      parseChar 'a' "aaaaabbb" `shouldBe` Just ('a', "aaaabbb")

    it "fails to parse when the first character doesn't match" $
      parseChar 'a' "bbbba" `shouldBe` Nothing

    it "fails to parse when the string is empty" $
      parseChar 'a' "" `shouldBe` Nothing

parseAnyCharSpec :: Spec
parseAnyCharSpec = do
  describe "parseAnyChar" $ do
    it "parses a character from the specified list correctly" $
      parseAnyChar "abc" "abc" `shouldBe` Just ('a', "bc")

    it "fails to parse a character not in the specified list" $
      parseAnyChar "abc" "xyz" `shouldBe` Nothing

    it "fails to parse when the input string is empty" $
      parseAnyChar "abc" "" `shouldBe` Nothing

    it "fails to parse when the first character doesn't match" $
      parseAnyChar "abc" "xyz" `shouldBe` Nothing

    it "parses the first character from a list with repeated characters" $
      parseAnyChar "abc" "abcbcd" `shouldBe` Just ('a', "bcbcd")

    it "handles parsing multiple characters from the specified list" $
      parseAnyChar "abc" "abcbcd" `shouldBe` Just ('a', "bcbcd")

    it "handles parsing when the first character is in the list" $
      parseAnyChar "abc" "abcd" `shouldBe` Just ('a', "bcd")

parseOrSpec :: Spec
parseOrSpec = do
  describe "parseOr" $ do
    it "parses from the first parser when successful" $
      parseOr (parseChar 'a') (parseChar 'b') "abc" `shouldBe` Just ('a', "bc")

    it "parses from the second parser when the first one fails" $
      parseOr (parseChar 'x') (parseChar 'y') "yz" `shouldBe` Just ('y', "z")

    it "fails when both parsers fail" $
      parseOr (parseChar 'x') (parseChar 'y') "abc" `shouldBe` Nothing

    it "handles parsing the last character in a string from the first parser" $
      parseOr (parseChar 'a') (parseChar 'b') "ab" `shouldBe` Just ('a', "b")

    it "handles parsing the last character in a string from the second parser" $
      parseOr (parseChar 'x') (parseChar 'y') "y" `shouldBe` Just ('y', "")

    it "fails when the input string is empty and both parsers fail" $
      parseOr (parseChar 'x') (parseChar 'y') "" `shouldBe` Nothing

    it "parses from the first parser when both succeed" $
      parseOr (parseChar 'a') (parseChar 'a') "abc" `shouldBe` Just ('a', "bc")

    it "parses from the first parser when both succeed with repeated characters" $
      parseOr (parseChar 'a') (parseChar 'a') "aaaabc" `shouldBe` Just ('a', "aaabc")

parseAndSpec :: Spec
parseAndSpec = do
  describe "parseAnd" $ do
    it "parses both parsers successfully" $
      parseAnd (parseChar 'a') (parseChar 'b') "abc" `shouldBe` Just (('a', 'b'), "c")

    it "fails when the first parser fails" $
      parseAnd (parseChar 'x') (parseChar 'y') "abc" `shouldBe` Nothing

    it "fails when the second parser fails" $
      parseAnd (parseChar 'a') (parseChar 'y') "abc" `shouldBe` Nothing

    it "fails when both parsers fail" $
      parseAnd (parseChar 'x') (parseChar 'y') "abc" `shouldBe` Nothing

    it "handles parsing the last character in a string successfully" $
      parseAnd (parseChar 'a') (parseChar 'b') "ab" `shouldBe` Just (('a', 'b'), "")

    it "fails when the input string is empty and the first parser fails" $
      parseAnd (parseChar 'x') (parseChar 'y') "" `shouldBe` Nothing

    it "fails when the input string is empty and the second parser fails" $
      parseAnd (parseChar 'a') (parseChar 'y') "" `shouldBe` Nothing

    it "fails when the input string is empty and both parsers fail" $
      parseAnd (parseChar 'x') (parseChar 'y') "" `shouldBe` Nothing

    it "parses from the first parser when both succeed with repeated characters" $
      parseAnd (parseChar 'a') (parseChar 'a') "aaaabc" `shouldBe` Just (('a', 'a'), "aabc")

parseSpec :: Spec
parseSpec = do
    describe "parse" $ do
      it "parses an empty string" $
        parse "" `shouldBe` []

      it "ignores spaces and newlines" $
        parse "  \n" `shouldBe` []

      it "parses a string" $
        parse "\"hello\"" `shouldBe` [StringLiteral "hello"]

parseIntSymSpec :: Spec
parseIntSymSpec = do
    describe "parseIntSym" $ do
      it "returns IntLiteral for positive integer string" $
        parseIntSym "42" `shouldBe` (IntLiteral 42 :: Ast)

      it "returns IntLiteral for negative integer string" $
        parseIntSym "-123" `shouldBe` (IntLiteral (-123) :: Ast)

      it "returns Symbol for non-integer string" $
        (show (parseIntSym "abc")) `shouldBe` (show (Symbol "abc" :: Ast))

parseStringSpec :: Spec
parseStringSpec = do
  describe "parseString" $ do
    it "parses an empty string" $
      parseString "" `shouldBe` ("", "")

    it "parses a simple string without escape characters" $
      parseString "hello" `shouldBe` ("hello", "")

    it "parses a string with an escaped quote" $
      parseString "a\\\"b" `shouldBe` ("a\"b", "")

    it "parses a string with multiple escaped quotes" $
      parseString "\\\"abc\\\"def\\\"" `shouldBe` ("\"abc\"def\"", "")

parseCallSpec :: Spec
parseCallSpec = do
  describe "parseCall" $ do
    it "parses an empty string" $
      parseCall "" `shouldBe` (Nothing, "")

    it "parses a simple function call with no arguments" $
      (show (parseCall "(add)")) `shouldBe`
        (show (Just $ Call "(add)" [], ""))

    it "parses a function call with one argument" $
      (show (parseCall "(subtract 5)")) `shouldBe`
        (show (Just $ Call "(subtract" [Symbol "5)"], ""))

    it "parses a function call with multiple arguments" $
      (show (parseCall "(multiply 2 3)")) `shouldBe`
        (show (Just $ Call "(multiply" [IntLiteral 2, Symbol "3)"], ""))

    it "handles a function call with nested calls" $
      (show (parseCall "(nested (add 1 2) (subtract 5))")) `shouldBe`
        (show (Just $ Call "(nested" [Call "add" [IntLiteral 1, IntLiteral 2], Call "subtract" [IntLiteral 5], Symbol ")"], ""))

parseListSpec :: Spec
parseListSpec = do
  describe "parseList" $ do
    it "parses an empty list" $
      parseList (0, "") `shouldBe` ([], "")

    it "parses a list with one element" $
      parseList (0, "(a)") `shouldBe` ("", "(a)")

    it "parses a nested list" $
      parseList (0, "(a (b c) d)") `shouldBe` ("", "(a (b c) d)")

    it "handles an empty string" $
      parseList (0, "") `shouldBe` ([], "")

    it "handles a list with multiple elements" $
      parseList (0, "(a b c)") `shouldBe` ("", "(a b c)")

    it "handles a nested list with multiple elements" $
      parseList (0, "(a (b c) d e)") `shouldBe` ("", "(a (b c) d e)")

    it "handles a nested list with extra parentheses" $
      parseList (0, "(a (b c) d))") `shouldBe` ("", "(a (b c) d))")

firstWordSpec :: Spec
firstWordSpec = do
  describe "firstWord" $ do
    it "returns the first word and the rest of the string" $
      firstWord "This is a test." `shouldBe` ("This", "is a test.")

    it "handles leading spaces" $
      firstWord "   Spaces at the beginning." `shouldBe` ("", "  Spaces at the beginning.")

    it "handles multiple spaces between words" $
      firstWord "Multiple    spaces between words." `shouldBe` ("Multiple", "   spaces between words.")

isStringNumberSpec :: Spec
isStringNumberSpec = do
  describe "isStringNumber" $ do
    it "returns False for an empty string" $
      isStringNumber "" `shouldBe` False

    it "returns True for a single-digit number" $
      isStringNumber "5" `shouldBe` True

    it "returns True for a multi-digit number" $
      isStringNumber "123" `shouldBe` True

    it "returns False for a string with non-digit characters" $
      isStringNumber "12a3" `shouldBe` False

    it "returns False for a string with spaces" $
      isStringNumber "1 2 3" `shouldBe` False

isNumberSpec :: Spec
isNumberSpec = do
  describe "isNumber" $ do
    it "returns True for a digit '5'" $
      isNumber '5' `shouldBe` True

    it "returns True for a digit '9'" $
      isNumber '9' `shouldBe` True

    it "returns False for a non-digit 'a'" $
      isNumber 'a' `shouldBe` False

    it "returns False for a non-digit '@'" $
      isNumber '@' `shouldBe` False

spec :: Spec
spec = do
    parseCharSpec
    parseAnyCharSpec
    parseOrSpec
    parseAndSpec
    parseSpec
    parseIntSymSpec
    parseStringSpec
    parseCallSpec
    parseListSpec
    firstWordSpec
    isStringNumberSpec
    isNumberSpec
