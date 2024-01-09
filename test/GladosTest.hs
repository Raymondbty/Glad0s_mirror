import Test.Hspec
import Ast
import Eval
import Funcs
import Test.QuickCheck
import Control.Exception (evaluate)
import Glados
import Parser

instance Eq SExpr where
  (SList a) == (SList b) = a == b
  (SSymbol a) == (SSymbol b) = a == b
  (SInt a) == (SInt b) = a == b
  -- Ajoutez d'autres cas si nécessaire

  -- Dans le cas général, considérez les autres cas comme différents
  _ == _ = False

instance Eq Ast where
  IntLiteral a == IntLiteral b = a == b
  StringLiteral a == StringLiteral b = a == b
  Define sym1 expr1 == Define sym2 expr2 = sym1 == sym2 && expr1 == expr2
  -- Ajoutez d'autres cas si nécessaire

  -- Dans le cas général, considérez les autres cas comme différents
  _ == _ = False

main :: IO ()
main = hspec $ do
  describe "getSymbol" $ do
    it "returns the symbol from SSymbol" $
      getSymbol (SSymbol "example") `shouldBe` Just "example"

    it "returns Nothing for SInt" $
      getSymbol (SInt 42) `shouldBe` Nothing

    it "returns Nothing for SList" $
      getSymbol (SList []) `shouldBe` Nothing

  describe "getInteger" $ do
    it "returns the integer from SInt" $
      getInteger (SInt 42) `shouldBe` Just 42

    it "returns Nothing for SSymbol" $
      getInteger (SSymbol "example") `shouldBe` Nothing

    it "returns Nothing for SList" $
      getInteger (SList []) `shouldBe` Nothing

  describe "getList" $ do
    it "returns the list from SList" $
      getList (SList [SSymbol "a", SSymbol "b"]) `shouldBe` Just [SSymbol "a", SSymbol "b"]

    it "returns Nothing for SSymbol" $
      getList (SSymbol "example") `shouldBe` Nothing

    it "returns Nothing for SInt" $
      getList (SInt 42) `shouldBe` Nothing

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

  describe "sexprToAST" $ do
    it "converts SInt to Ast correctly" $
      sexprToAST (SInt 42) `shouldBe` Just (IntLiteral 42)

    it "converts SSymbol to Ast correctly" $
      sexprToAST (SSymbol "example") `shouldBe` Just (StringLiteral "example")

    it "converts SList to Ast correctly" $
      sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 42]) `shouldBe` Just (Define "x" (IntLiteral 42))

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

  describe "evalAST" $ do
    it "evaluates addition correctly" $
      evalAST (Call "+" [IntLiteral 2, IntLiteral 3]) `shouldBe` Right (IntLiteral 5)

    it "handles unrecognized function calls" $
      evalAST (Call "unknown" []) `shouldBe` Left "no matching function 'unknown' (unknown)"

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

  describe "if cond" $ do
    it "returns the true branch for true condition" $
      if_cond (Right (Call "if" [BoolLiteral True, IntLiteral 42, IntLiteral 24]))
      `shouldBe` Right (IntLiteral 42)

    it "returns the false branch for false condition" $
      if_cond (Right (Call "if" [BoolLiteral False, IntLiteral 42, IntLiteral 24]))
      `shouldBe` Right (IntLiteral 24)

    it "returns an error for incorrect arguments" $
      if_cond (Right (Call "if" [BoolLiteral True, IntLiteral 42]))
      `shouldBe` Left "function 'if' require three arguments (if True 42)"

    describe "parse" $ do
      it "parses an empty string" $
        parse "" `shouldBe` []

      it "ignores spaces and newlines" $
        parse "  \n" `shouldBe` []

      it "parses a string" $
        parse "\"hello\"" `shouldBe` [StringLiteral "hello"]
