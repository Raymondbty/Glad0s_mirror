--
-- EPITECH PROJECT, 2024
-- B-FUN-500-PAR-5-2-glados-hugo.mouraud
-- File description:
-- VMSpec
--

module VMSpec (spec) where

import Test.Hspec
import Types
import VM
import Instances()

vmSpec :: Spec
vmSpec = do
    describe "execOpADD" $ do
        it "returns an error when ADD operation has insufficient operands" $
            execOpADD [CallOp ADD] [IntVM 5] `shouldBe` Left "ADD need two integers"

        it "returns an error when ADD operation has non-integer operands" $
            execOpADD [Push (BoolVM True), Push (IntVM 4), CallOp ADD] [IntVM 5] `shouldBe` Left "ADD need two integers"
    describe "execOpSUB" $ do
        it "returns an error when SUB operation has insufficient operands" $
            execOpSUB [CallOp SUB] [IntVM 5] `shouldBe` Left "SUB need two integers"

        it "returns an error when SUB operation has non-integer operands" $
            execOpSUB [Push (BoolVM True), Push (IntVM 4), CallOp  SUB] [IntVM 5] `shouldBe` Left "SUB need two integers"
    describe "execOpMUL" $ do
        it "returns an error when MUL operation has insufficient operands" $
            execOpMUL [CallOp MUL] [IntVM 5] `shouldBe` Left "MUL need two integers"

        it "returns an error when MUL operation has non-integer operands" $
            execOpMUL [Push (BoolVM True), Push (IntVM 4), CallOp MUL] [IntVM 5] `shouldBe` Left "MUL need two integers"

    describe "execOpDIV" $ do
        it "returns an error when DIV operation has insufficient operands" $
            execOpDIV [CallOp DIV] [IntVM 5] `shouldBe` Left "DIV need two integers"

        it "returns an error when DIV operation has non-integer operands" $
            execOpDIV [Push (BoolVM True), Push (IntVM 4), CallOp DIV] [IntVM 5] `shouldBe` Left "DIV need two integers"

        it "returns an error when attempting to divide by zero" $
            execOpDIV [Push (IntVM 10), Push (IntVM 0), CallOp DIV] [IntVM 5, IntVM 7] `shouldBe` Left "no value to return"

    describe "execOpMOD" $ do
        it "returns an error when MOD operation has insufficient operands" $
            execOpMOD [CallOp MOD] [IntVM 5] `shouldBe` Left "MOD need two integers"

        it "returns an error when MOD operation has non-integer operands" $
            execOpMOD [Push (BoolVM True), Push (IntVM 4), CallOp MOD] [IntVM 5] `shouldBe` Left "MOD need two integers"

        it "returns an error when attempting to calculate modulo by zero" $
            execOpMOD [Push (IntVM 10), Push (IntVM 0), CallOp MOD] [IntVM 5, IntVM 7] `shouldBe` Left "no value to return"

    describe "execOpEQUAL" $ do
        it "returns an error when EQUAL operation has insufficient operands" $
            execOpEQUAL [CallOp EQUAL] [BoolVM True] `shouldBe` Left "EQUAL need two arguments"

        it "returns an error when EQUAL operation has non-matching types" $
            execOpEQUAL [Push (IntVM 42), Push (BoolVM True), CallOp EQUAL] [BoolVM False] `shouldBe` Left "EQUAL need two arguments"

    describe "execOpLESS" $ do
        it "returns an error when LESS operation has insufficient operands" $
            execOpLESS [CallOp LESS] [BoolVM True] `shouldBe` Left "LESS need two arguments"

        it "returns an error when LESS operation has non-matching types" $
            execOpLESS [Push (IntVM 42), Push (BoolVM True), CallOp LESS] [BoolVM False] `shouldBe` Left "LESS need two arguments"

    describe "remInst" $ do
        it "removes the specified number of instructions from the beginning of the list" $
            remInst 2 [Push (IntVM 1), Push (IntVM 2), CallOp ADD, Ret] `shouldBe` [CallOp ADD, Ret]

        it "returns an empty list when removing all instructions" $
            remInst 3 [Push (IntVM 1), Push (IntVM 2), CallOp ADD] `shouldBe` []

        it "returns the same list when removing zero instructions" $
            remInst 0 [Push (IntVM 1), Push (IntVM 2), CallOp ADD, Ret] `shouldBe` [Push (IntVM 1), Push (IntVM 2), CallOp ADD, Ret]

    describe "execJumpIfFalse" $ do
        it "does not jump when the condition is true (IntVM)" $
            execJumpIfFalse 2 [Push (IntVM 1),  JUMPIFFALSE 2, Push (IntVM 42), Ret] [] `shouldBe` Right (IntVM 42)

        it "returns an error when the stack does not contain the expected values" $
            execJumpIfFalse 2 [Push (BoolVM False),  JUMPIFFALSE 2, Push (IntVM 42), Ret] [IntVM 5] `shouldBe` Left "no value to return"

    describe "exec" $ do
        it "executes JUMPIFFALSE correctly when the condition is false" $
            exec [JUMPIFFALSE 2, Push (BoolVM False), Push (IntVM 42), Ret] [] `shouldBe` Right (IntVM 42)

        it "returns an error when there is no value to return" $
            exec [Ret] [] `shouldBe` Left "no value to return"

    describe "parseVMCall" $ do
        it "parses 'ADD' correctly" $
            parseVMCall "ADD" `shouldBe` Just (CallOp ADD)

        it "parses 'SUB' correctly" $
            parseVMCall "SUB" `shouldBe` Just (CallOp SUB)

        it "parses 'MUL' correctly" $
            parseVMCall "MUL" `shouldBe` Just (CallOp MUL)

        it "parses 'DIV' correctly" $
            parseVMCall "DIV" `shouldBe` Just (CallOp DIV)

        it "parses 'MOD' correctly" $
            parseVMCall "MOD" `shouldBe` Just (CallOp MOD)

        it "parses 'EQUAL' correctly" $
            parseVMCall "EQUAL" `shouldBe` Just (CallOp EQUAL)

        it "parses 'LESS' correctly" $
            parseVMCall "LESS" `shouldBe` Just (CallOp LESS)

        it "returns Nothing for unknown string" $
            parseVMCall "UNKNOWN" `shouldBe` Nothing

        it "returns Nothing for an empty string" $
            parseVMCall "" `shouldBe` Nothing

    describe "parseVMPush" $ do
        it "parses 'True' correctly" $
            parseVMPush "True" `shouldBe` Just (Push $ BoolVM True)

        it "parses 'False' correctly" $
            parseVMPush "False" `shouldBe` Just (Push $ BoolVM False)

        it "parses positive integer correctly" $
            parseVMPush "42" `shouldBe` Just (Push $ IntVM 42)

        it "parses negative integer correctly" $
            parseVMPush "-123" `shouldBe` Just (Push $ IntVM (-123))

        it "returns Nothing for non-integer string" $
            parseVMPush "abc" `shouldBe` Nothing

        it "returns Nothing for an empty string" $
            parseVMPush "" `shouldBe` Nothing

    describe "parseJump" $ do
        it "parses integer correctly" $
            parseJump "42" `shouldBe` Just 42

        it "returns Nothing for non-integer string" $
            parseJump "abc" `shouldBe` Nothing

        it "returns Nothing for an empty string" $
            parseJump "" `shouldBe` Nothing

    describe "parseVM" $ do
        it "parses CALL instruction correctly" $
            parseVM "CALL ADD" `shouldBe` [CallOp ADD]

        it "parses PUSH instruction correctly" $
            parseVM "PUSH True" `shouldBe` [Push (BoolVM True)]

        it "parses RET instruction correctly" $
            parseVM "RET" `shouldBe` [Ret]

        it "parses JUMPIFFALSE instruction correctly" $
            parseVM "JUMPIFFALSE 42" `shouldBe` [JUMPIFFALSE 42]

        it "parses JUMP instruction correctly" $
            parseVM "JUMP 123" `shouldBe` [JUMP 123]

        it "handles unknown instructions correctly" $
            parseVM "UNKNOWN 42" `shouldBe` []

        it "handles empty input correctly" $
            parseVM "" `shouldBe` []

spec :: Spec
spec = do
    vmSpec