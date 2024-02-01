--
-- EPITECH PROJECT, 2024
-- B-FUN-500-PAR-5-2-glados-hugo.mouraud
-- File description:
-- ParserUtils
--

module ParserUtilsSpec (spec) where

import Test.Hspec
import Types
import ParserUtils
import Instances()

parserUtilsSpec :: Spec
parserUtilsSpec = do
    describe "parseChar" $ do
        it "parses the correct character" $ do
            let input = "abc"
            let result = runParser (parseChar 'a') input
            result `shouldBe` Just ('a', "bc")

        it "fails to parse incorrect character" $ do
            let input = "abc"
            let result = runParser (parseChar 'x') input
            result `shouldBe` Nothing

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser (parseChar 'a') input
            result `shouldBe` Nothing

        it "parses character and leaves trailing characters" $ do
            let input = "a123"
            let result = runParser (parseChar 'a') input
            result `shouldBe` Just ('a', "123")
    describe "isInList" $ do
        it "returns True when the character is in the list" $ do
            isInList 'a' "abc" `shouldBe` True

        it "returns False when the character is not in the list" $ do
            isInList 'x' "abc" `shouldBe` False

        it "returns False for an empty list" $ do
            isInList 'a' "" `shouldBe` False

        it "returns True for a single-character list with a matching character" $ do
            isInList 'a' "a" `shouldBe` True

        it "returns False for a single-character list with a non-matching character" $ do
            isInList 'a' "b" `shouldBe` False
    describe "parseAnyChar" $ do
        it "parses the correct character from the list" $ do
            let input = "abc"
            let result = runParser (parseAnyChar "abc") input
            result `shouldBe` Just ('a', "bc")

        it "fails to parse character not in the list" $ do
            let input = "abc"
            let result = runParser (parseAnyChar "xyz") input
            result `shouldBe` Nothing

        it "fails to parse from an empty string" $ do
            let input = ""
            let result = runParser (parseAnyChar "abc") input
            result `shouldBe` Nothing

        it "parses character and leaves trailing characters" $ do
            let input = "a123"
            let result = runParser (parseAnyChar "a") input
            result `shouldBe` Just ('a', "123")
    describe "parseMany" $ do
        it "parses multiple occurrences of a character" $ do
            let input = "aaabbb"
            let result = runParser (parseMany (parseChar 'a')) input
            result `shouldBe` Just ("aaa", "bbb")

        it "parses an empty list if no occurrences are found" $ do
            let input = "bbb"
            let result = runParser (parseMany (parseChar 'a')) input
            result `shouldBe` Just ("", "bbb")

        it "parses an empty list from an empty string" $ do
            let input = ""
            let result = runParser (parseMany (parseChar 'a')) input
            result `shouldBe` Just ([], "")
    describe "parseSome" $ do
        it "parses multiple occurrences of a character" $ do
            let input = "aaabbb"
            let result = runParser (parseSome (parseChar 'a')) input
            result `shouldBe` Just ("aaa", "bbb")

        it "fails if no occurrences are found" $ do
            let input = "bbb"
            let result = runParser (parseSome (parseChar 'a')) input
            result `shouldBe` Nothing

        it "fails for an empty string" $ do
            let input = ""
            let result = runParser (parseSome (parseChar 'a')) input
            result `shouldBe` Nothing
    describe "parseNumber" $ do
        it "parses a positive integer" $ do
            let input = "123"
            let result = runParser parseNumber input
            result `shouldBe` Just ("123", "")

        it "parses a negative integer" $ do
            let input = "-456"
            let result = runParser parseNumber input
            result `shouldBe` Just ("-456", "")

        it "fails to parse an integer with leading zeros" $ do
            let input = "007"
            let result = runParser parseNumber input
            result `shouldBe` Just ("007","")

        it "fails to parse a non-integer string" $ do
            let input = "abc"
            let result = runParser parseNumber input
            result `shouldBe` Nothing

        it "parses a single-digit integer" $ do
            let input = "9"
            let result = runParser parseNumber input
            result `shouldBe` Just ("9", "")

        it "parses an integer with leading negative sign" $ do
            let input = "-789"
            let result = runParser parseNumber input
            result `shouldBe` Just ("-789", "")
    describe "parseVar" $ do
        it "parses a valid variable name" $ do
            let input = "myVar123"
            let result = runParser parseVar input
            result `shouldBe` Just ("myVar123", "")

        it "parses a variable name starting with an underscore" $ do
            let input = "_variable"
            let result = runParser parseVar input
            result `shouldBe` Just ("_variable", "")

        it "fails to parse a variable name starting with a digit" $ do
            let input = "123variable"
            let result = runParser parseVar input
            result `shouldBe` Nothing

        it "parses a single-character variable name" $ do
            let input = "x"
            let result = runParser parseVar input
            result `shouldBe` Just ("x", "")

        it "fails to parse an invalid variable name with special characters" $ do
            let input = "@invalidVar"
            let result = runParser parseVar input
            result `shouldBe` Nothing

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseVar input
            result `shouldBe` Nothing
    describe "parseNumberVar" $ do
        it "parses a valid variable name starting with a letter" $ do
            let input = "myVar123"
            let result = runParser parseNumberVar input
            result `shouldBe` Just ("my", "Var123")

        it "parses a valid variable name starting with an underscore" $ do
            let input = "_variable"
            let result = runParser parseNumberVar input
            result `shouldBe` Just ("_variable", "")

        it "parses a valid variable name starting with a digit" $ do
            let input = "123variable"
            let result = runParser parseNumberVar input
            result `shouldBe` Just ("123variable", "")

        it "parses a single-character variable name" $ do
            let input = "x"
            let result = runParser parseNumberVar input
            result `shouldBe` Just ("x", "")

        it "fails to parse an invalid variable name starting with a special character" $ do
            let input = "@invalidVar"
            let result = runParser parseNumberVar input
            result `shouldBe` Nothing

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseNumberVar input
            result `shouldBe` Nothing
    describe "parseStr" $ do
        it "parses a simple string" $ do
            let input = "\"Hello, World!\""
            let result = runParser parseStr input
            result `shouldBe` Just ("", "\"Hello, World!\"")

        it "parses a string with escaped quotes" $ do
            let input = "\"Escape \\\"quotes\\\" here.\""
            let result = runParser parseStr input
            result `shouldBe` Just ("", "\"Escape \\\"quotes\\\" here.\"")

        it "fails to parse an unclosed string" $ do
            let input = "\"Unclosed string"
            let result = runParser parseStr input
            result `shouldBe` Just ("","\"Unclosed string")

        it "parses an empty string" $ do
            let input = "\"\""
            let result = runParser parseStr input
            result `shouldBe` Just ("", "\"\"")

        it "parses a string with special characters" $ do
            let input = "\"@#$%^&*()\""
            let result = runParser parseStr input
            result `shouldBe` Just ("", "\"@#$%^&*()\"")

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseStr input
            result `shouldBe` Nothing
    describe "parseSep" $ do
        it "parses a semicolon after spaces" $ do
            let input = "   ; rest of string"
            let result = runParser parseSep input
            result `shouldBe` Just ("   ", " rest of string")

        it "parses a semicolon after spaces and other characters" $ do
            let input = "   ;  rest of string"
            let result = runParser parseSep input
            result `shouldBe` Just ("   ", "  rest of string")

        it "fails to parse without spaces before semicolon" $ do
            let input = ";rest of string"
            let result = runParser parseSep input
            result `shouldBe` Just ("","rest of string")

        it "fails to parse without semicolon" $ do
            let input = "   rest of string"
            let result = runParser parseSep input
            result `shouldBe` Nothing

        it "parses a semicolon at the beginning of the string" $ do
            let input = ";rest of string"
            let result = runParser parseSep input
            result `shouldBe` Just ("", "rest of string")

        it "parses a semicolon with leading and trailing spaces" $ do
            let input = "  ;  rest of string"
            let result = runParser parseSep input
            result `shouldBe` Just ("  ", "  rest of string")

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseSep input
            result `shouldBe` Nothing
    describe "parseSpaces" $ do
        it "parses spaces at the beginning of the string" $ do
            let input = "   rest of string"
            let result = runParser parseSpaces input
            result `shouldBe` Just ("   ", "rest of string")

        it "parses spaces between characters" $ do
            let input = "abc   def"
            let result = runParser parseSpaces input
            result `shouldBe` Just ("", "abc   def")

        it "parses multiple spaces" $ do
            let input = "    "
            let result = runParser parseSpaces input
            result `shouldBe` Just ("    ", "")

        it "fails to parse without spaces" $ do
            let input = "no spaces here"
            let result = runParser parseSpaces input
            result `shouldBe` Just ("","no spaces here")

        it "parses spaces at the end of the string" $ do
            let input = "rest of string   "
            let result = runParser parseSpaces input
            result `shouldBe` Just ("", "rest of string   ")

        it "parses spaces in an empty string" $ do
            let input = ""
            let result = runParser parseSpaces input
            result `shouldBe` Just ("", "")
    describe "parseListArgs" $ do
        it "parses an empty argument list" $ do
            let input = ")"
            let result = runParser parseListArgs input
            result `shouldBe` Just ([], ")")

        it "parses a single argument without spaces" $ do
            let input = "arg)"
            let result = runParser parseListArgs input
            result `shouldBe` Just (["arg"], ")")

        it "parses multiple arguments with spaces" $ do
            let input = "arg1 , arg2 , arg3 )"
            let result = runParser parseListArgs input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse with missing closing parenthesis" $ do
            let input = "arg1 , arg2 , arg3 "
            let result = runParser parseListArgs input
            result `shouldBe` Nothing


        it "fails to parse with invalid characters" $ do
            let input = "arg1 , arg2 , , arg3 )"
            let result = runParser parseListArgs input
            result `shouldBe` Nothing

        it "parses an argument list with leading spaces" $ do
            let input = "   arg1 ,   arg2 , arg3 )"
            let result = runParser parseListArgs input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

    describe "parseListArgsIf" $ do
        it "parses an empty argument list" $ do
            let input = ")"
            let result = runParser parseListArgsIf input
            result `shouldBe` Just ([], ")")

        it "parses a single argument without spaces" $ do
            let input = "arg)"
            let result = runParser parseListArgsIf input
            result `shouldBe` Just (["arg"], ")")

        it "parses multiple arguments with spaces" $ do
            let input = "arg1 , arg2 , arg3 )"
            let result = runParser parseListArgsIf input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse with missing closing parenthesis" $ do
            let input = "arg1 , arg2 , arg3 "
            let result = runParser parseListArgsIf input
            result `shouldBe` Nothing

        it "fails to parse with invalid characters" $ do
            let input = "arg1 , arg2 , , arg3 )"
            let result = runParser parseListArgsIf input
            result `shouldBe` Nothing

        it "parses an argument list with leading spaces" $ do
            let input = "   arg1 ,   arg2 , arg3 )"
            let result = runParser parseListArgsIf input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "parses arguments with leading digits" $ do
            let input = "123 , 456 , 789 )"
            let result = runParser parseListArgsIf input
            result `shouldBe` Just (["123", "456", "789"], ")")

    describe "parseList" $ do
        it "parses an empty list" $ do
            let input = ")"
            let result = runParser parseList input
            result `shouldBe` Just ([], ")")

        it "parses a list with arguments" $ do
            let input = "arg1 , arg2 , arg3 )"
            let result = runParser parseList input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse with missing closing parenthesis" $ do
            let input = "arg1 , arg2 , arg3 "
            let result = runParser parseList input
            result `shouldBe` Nothing

        it "parses a list with leading spaces" $ do
            let input = "   arg1 ,   arg2 , arg3 )"
            let result = runParser parseList input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseList input
            result `shouldBe` Nothing

        it "fails to parse with invalid characters" $ do
            let input = "arg1 , arg2 , , arg3 )"
            let result = runParser parseList input
            result `shouldBe` Nothing

    describe "parseListIf" $ do
        it "parses an empty list" $ do
            let input = ")"
            let result = runParser parseListIf input
            result `shouldBe` Just ([], ")")

        it "parses a list with arguments" $ do
            let input = "arg1 , arg2 , arg3 )"
            let result = runParser parseListIf input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse with missing closing parenthesis" $ do
            let input = "arg1 , arg2 , arg3 "
            let result = runParser parseListIf input
            result `shouldBe` Nothing

        it "parses a list with leading spaces" $ do
            let input = "   arg1 ,   arg2 , arg3 )"
            let result = runParser parseListIf input
            result `shouldBe` Just (["arg1", "arg2", "arg3"], ")")

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser parseListIf input
            result `shouldBe` Nothing

        it "fails to parse with invalid characters" $ do
            let input = "arg1 , arg2 , , arg3 )"
            let result = runParser parseListIf input
            result `shouldBe` Nothing

        it "parses a list with numbers" $ do
            let input = "123 , 456 , 789 )"
            let result = runParser parseListIf input
            result `shouldBe` Just (["123", "456", "789"], ")")

        it "parses a list with leading digits" $ do
            let input = "123 , 456 , 789 , )"
            let result = runParser parseListIf input
            result `shouldBe` Just (["123", "456", "789"], ")")

    describe "parseWord" $ do
        it "parses an empty word" $ do
            let input = "restOfString"
            let result = runParser (parseWord "") input
            result `shouldBe` Just ("", input)

        it "parses a single character word" $ do
            let input = "wordrestOfString"
            let result = runParser (parseWord "w") input
            result `shouldBe` Just ("w", "ordrestOfString")

        it "parses a word at the beginning of the string" $ do
            let input = "word restOfString"
            let result = runParser (parseWord "word") input
            result `shouldBe` Just ("word", " restOfString")

        it "fails to parse a word not present in the string" $ do
            let input = "restOfString"
            let result = runParser (parseWord "word") input
            result `shouldBe` Nothing

        it "fails to parse a word with a mismatched character" $ do
            let input = "sword restOfString"
            let result = runParser (parseWord "word") input
            result `shouldBe` Nothing

        it "fails to parse an empty string" $ do
            let input = ""
            let result = runParser (parseWord "word") input
            result `shouldBe` Nothing

        it "parses a word with multiple characters" $ do
            let input = "helloworld restOfString"
            let result = runParser (parseWord "helloworld") input
            result `shouldBe` Just ("helloworld", " restOfString")

spec :: Spec
spec = do
    parserUtilsSpec
