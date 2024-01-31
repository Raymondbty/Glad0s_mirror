--
-- EPITECH PROJECT, 2024
-- B-FUN-500-PAR-5-2-glados-hugo.mouraud
-- File description:
-- ParserSpec
--

module ParserSpec (spec) where

import Test.Hspec
import Types
import Parser
import Instances()

parserSpec :: Spec
parserSpec = do
    describe "parseBool" $ do
        it "fails to parse invalid input" $ do
            let input = "invalid"
            let result = runParser parseBool input
            result `shouldBe` Nothing
    describe "parseInt" $ do
        it "parses positive integer correctly" $ do
            let input = "123"
            let result = runParser parseInt input
            result `shouldBe` Just (IntLiteral 123, "")

        it "parses negative integer correctly" $ do
            let input = "-456"
            let result = runParser parseInt input
            result `shouldBe` Just (IntLiteral (-456), "")

        it "fails to parse non-integer input" $ do
            let input = "abc"
            let result = runParser parseInt input
            result `shouldBe` Nothing

        it "fails to parse if no digits are present" $ do
            let input = "   "
            let result = runParser parseInt input
            result `shouldBe` Nothing
    describe "parseSymbol" $ do

        it "fails to parse invalid symbol starting with a digit" $ do
            let input = "123symbol"
            let result = runParser parseSymbol input
            result `shouldBe` Nothing

        it "fails to parse empty input" $ do
            let input = ""
            let result = runParser parseSymbol input
            result `shouldBe` Nothing

    describe "parseStringContent" $ do
        it "parses string content correctly" $ do
            let input = "\"Hello, World!\""
            let result = runParser parseStringContent input
            result `shouldBe` Just (StringLiteral "", "\"Hello, World!\"")

        it "fails to parse unterminated string" $ do
            let input = "\"Hello, World!"
            let result = runParser parseStringContent input
            result `shouldBe` Just (StringLiteral "","\"Hello, World!")

        it "parses string with escaped characters" $ do
            let input = "\"Newline: \\n, Tab: \\t\""
            let result = runParser parseStringContent input
            result `shouldBe` Just (StringLiteral "", "\"Newline: \\n, Tab: \\t\"")

        it "handles string with trailing spaces" $ do
            let input = "\"String   \""
            let result = runParser parseStringContent input
            result `shouldBe` Just (StringLiteral "", "\"String   \"")
    describe "parseString" $ do
        it "parses a simple string correctly" $ do
            let input = "\"Hello, World!\""
            let result = runParser parseString input
            result `shouldBe` Just (StringLiteral "Hello, World!", "")

        it "parses a string with escaped characters" $ do
            let input = "\"Newline: \\n, Tab: \\t\""
            let result = runParser parseString input
            result `shouldBe` Just (StringLiteral "Newline: \\n, Tab: \\t", "")

        it "fails to parse an unterminated string" $ do
            let input = "\"Hello, World!"
            let result = runParser parseString input
            result `shouldBe` Nothing

        it "parses a string and leaves trailing spaces" $ do
            let input = "\"String   \""
            let result = runParser parseString input
            result `shouldBe` Just (StringLiteral "String   ", "")

        it "fails to parse an empty string" $ do
            let input = "\"\""
            let result = runParser parseString input
            result `shouldBe` Just (StringLiteral "", "")
    describe "parseFuncContent" $ do
        it "fails to parse invalid function content" $ do
            let input = "{ x = 42; }"
            let result = runParser (parseFuncContent "test2" ["x", "y"]) input
            result `shouldBe` Nothing

spec :: Spec
spec = do
    parserSpec