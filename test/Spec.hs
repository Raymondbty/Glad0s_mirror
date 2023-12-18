import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import EvalAst
import ParserList

main :: IO ()
main = hspec $ do
  describe "evalAST" $ do
    it "evaluates integer literals correctly" $
      evalAST (IntegerLiteral 42) `shouldBe` Just (IntegerLiteral 42)

    it "evaluates addition correctly" $
      evalAST (Call "+" [IntegerLiteral 2, IntegerLiteral 3]) `shouldBe` Just (IntegerLiteral 5)

    it "evaluates multiplication correctly" $
      evalAST (Call "*" [IntegerLiteral 2, IntegerLiteral 3]) `shouldBe` Just (IntegerLiteral 6)

    it "evaluates division correctly" $
      evalAST (Call "/" [IntegerLiteral 6, IntegerLiteral 2]) `shouldBe` Just (IntegerLiteral 3)

    it "handles division by zero" $
      evaluate (evalAST (Call "/" [IntegerLiteral 6, IntegerLiteral 0])) `shouldThrow` anyException

    it "evaluates subtraction correctly" $
      evalAST (Call "-" [IntegerLiteral 5, IntegerLiteral 2]) `shouldBe` Just (IntegerLiteral 3)

    it "evaluates 'if' condition correctly" $
      evalAST (Call "if" [BooleanLiteral True, IntegerLiteral 42, IntegerLiteral 0]) `shouldBe` Just (IntegerLiteral 42)

    it "handles unrecognized function calls" $
      evalAST (Call "unknown" []) `shouldBe` Nothing

  describe "parseInt" $ do
    it "parses a single digit" $
      runParser parseInt "5" `shouldBe` Just (5, "")

    it "parses multiple digits" $
      runParser parseInt "123" `shouldBe` Just (123, "")

    it "fails on non-digit input" $
      runParser parseInt "abc" `shouldBe` Nothing

  describe "char" $ do
    it "parses the specified character" $
      runParser (char 'a') "abc" `shouldBe` Just ('a', "bc")

    it "fails if the specified character is not present" $
      runParser (char 'a') "xyz" `shouldBe` Nothing

  describe "spaces" $ do
    it "parses spaces" $
      runParser spaces "  abc" `shouldBe` Just ("  ", "abc")

    it "parses no spaces" $
      runParser spaces "abc" `shouldBe` Just ("", "abc")

  describe "parseList" $ do
    it "parses a valid list of integers" $
      runParser (parseList parseInt) "(1, 2, 3, 5, 7)" `shouldBe` Just ([1, 2, 3, 5, 7], "")

    it "parses an empty list" $
      runParser (parseList parseInt) "()" `shouldBe` Just ([], "")

    it "fails on invalid input" $
      runParser (parseList parseInt) "(1, 2, abc)" `shouldBe` Nothing

    it "parses a list with spaces" $
      runParser (parseList parseInt) "(1 , 2 , 3 , 4)" `shouldBe` Just ([1, 2, 3, 4], "")

    it "parses a list with multiple spaces" $
      runParser (parseList parseInt) "(1  ,  2  ,  3  ,  4)" `shouldBe` Just ([1, 2, 3, 4], "")
