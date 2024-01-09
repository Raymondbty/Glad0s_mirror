{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- FuncsSpec.hs
-}


module FuncsSpec (spec) where

import Test.Hspec
import Ast
import Funcs
import Instances()

equalSpec :: Spec
equalSpec = do
    describe "equal" $ do
      it "returns Left for error" $
        equal (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right BoolLiteral True for equal integers" $
        (show (equal (Right (Call "eq?" [IntLiteral 42, IntLiteral 42])))) `shouldBe` (show (Right (BoolLiteral True) :: Either String Ast))

      it "returns Right BoolLiteral False for different integers" $
        (show (equal (Right (Call "eq?" [IntLiteral 42, IntLiteral 24])))) `shouldBe` (show (Right (BoolLiteral False) :: Either String Ast))

      it "returns Right BoolLiteral True for equal strings" $
        (show (equal (Right (Call "eq?" [StringLiteral "hello", StringLiteral "hello"])))) `shouldBe` (show (Right (BoolLiteral True) :: Either String Ast))

      it "returns Right BoolLiteral False for different strings" $
        (show (equal (Right (Call "eq?" [StringLiteral "hello", StringLiteral "world"])))) `shouldBe` (show (Right (BoolLiteral False) :: Either String Ast))

      it "returns Left for wrong argument types" $
        equal (Right (Call "eq?" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` Left "function 'eq?' require two arguments (eq? 42 \"hello\")"

lowerSpec :: Spec
lowerSpec = do
    describe "lower" $ do
      it "returns Left for error" $
        lower (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right BoolLiteral True for lower integers" $
        (show (lower (Right (Call "<" [IntLiteral 42, IntLiteral 50])))) `shouldBe` (show (Right (BoolLiteral True) :: Either String Ast))

      it "returns Right BoolLiteral False for non-lower integers" $
        (show (lower (Right (Call "<" [IntLiteral 50, IntLiteral 42])))) `shouldBe` (show (Right (BoolLiteral False) :: Either String Ast))

      it "returns Left for wrong argument types" $
        lower (Right (Call "<" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '<' require two arguments (< 42 \"hello\")" :: Either String Ast)

plusSpec :: Spec
plusSpec = do
    describe "plus" $ do
      it "returns Left for error" $
        plus (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right IntLiteral for addition of integers" $
        plus (Right (Call "+" [IntLiteral 2, IntLiteral 3])) `shouldBe` (Right (IntLiteral 5) :: Either String Ast)

      it "returns Left for wrong argument types" $
        plus (Right (Call "+" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '+' require two arguments (+ 42 \"hello\")" :: Either String Ast)

minusSpec :: Spec
minusSpec = do
    describe "minus" $ do
      it "returns Left for error" $
        minus (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right IntLiteral for subtraction of integers" $
        minus (Right (Call "-" [IntLiteral 5, IntLiteral 3])) `shouldBe` (Right (IntLiteral 2) :: Either String Ast)

      it "returns Left for wrong argument types" $
        minus (Right (Call "-" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '-' require two arguments (- 42 \"hello\")" :: Either String Ast)

mulSpec :: Spec
mulSpec = do
    describe "mul" $ do
      it "returns Left for error" $
        mul (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right IntLiteral for multiplication of integers" $
        mul (Right (Call "*" [IntLiteral 2, IntLiteral 3])) `shouldBe` (Right (IntLiteral 6) :: Either String Ast)

      it "returns Left for wrong argument types" $
        mul (Right (Call "*" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '*' require two arguments (* 42 \"hello\")" :: Either String Ast)

myDivSpec :: Spec
myDivSpec = do
    describe "myDiv" $ do
      it "returns Left for error" $
        myDiv (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right IntLiteral for division of non-zero integers" $
        myDiv (Right (Call "/" [IntLiteral 6, IntLiteral 3])) `shouldBe` (Right (IntLiteral 2) :: Either String Ast)

      it "returns Right IntLiteral 0 for division by 0" $
        myDiv (Right (Call "/" [IntLiteral 5, IntLiteral 0])) `shouldBe` (Right (IntLiteral 0) :: Either String Ast)

      it "returns Left for wrong argument types" $
        myDiv (Right (Call "/" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '/' require two arguments (/ 42 \"hello\")" :: Either String Ast)

myModSpec :: Spec
myModSpec = do
    describe "myMod" $ do
      it "returns Left for error" $
        myMod (Left "an error message") `shouldBe` Left "an error message"

      it "returns Right IntLiteral for modulo of non-zero integers" $
        myMod (Right (Call "%" [IntLiteral 7, IntLiteral 3])) `shouldBe` (Right (IntLiteral 1) :: Either String Ast)

      it "returns Right IntLiteral 0 for modulo by 0" $
        myMod (Right (Call "%" [IntLiteral 5, IntLiteral 0])) `shouldBe` (Right (IntLiteral 0) :: Either String Ast)

      it "returns Left for wrong argument types" $
        myMod (Right (Call "%" [IntLiteral 42, StringLiteral "hello"])) `shouldBe` (Left "function '%' require two arguments (% 42 \"hello\")" :: Either String Ast)

spec :: Spec
spec = do
    equalSpec
    lowerSpec
    plusSpec
    minusSpec
    mulSpec
    myDivSpec
    myModSpec
