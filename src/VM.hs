{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- VM.hs
-}

module VM (startVM) where

import Control.Exception
import Parser
import System.Exit
import Types

execOpADD :: Insts -> Stack -> Either String Value
execOpADD insts ((IntVM i1):(IntVM i2):stack) = exec insts ((IntVM $ i1 + i2) : stack)
execOpADD _ _ = Left "ADD need two integers"

execOpSUB :: Insts -> Stack -> Either String Value
execOpSUB insts ((IntVM i1):(IntVM i2):stack) = exec insts ((IntVM $ i1 - i2) : stack)
execOpSUB _ _ = Left "SUB need two integers"

execOpMUL :: Insts -> Stack -> Either String Value
execOpMUL insts ((IntVM i1):(IntVM i2):stack) = exec insts ((IntVM $ i1 * i2) : stack)
execOpMUL _ _ = Left "MUL need two integers"

execOpDIV :: Insts -> Stack -> Either String Value
execOpDIV _ ((IntVM _):(IntVM 0):_) = Left "division by 0"
execOpDIV insts ((IntVM i1):(IntVM i2):stack) = exec insts ((IntVM $ i1 `div` i2) : stack)
execOpDIV _ _ = Left "DIV need two integers"

execOpMOD :: Insts -> Stack -> Either String Value
execOpMOD _ ((IntVM _):(IntVM 0):_) = Left "modulo by 0"
execOpMOD insts ((IntVM i1):(IntVM i2):stack) = exec insts ((IntVM $ i1 `mod` i2) : stack)
execOpMOD _ _ = Left "MOD need two integers"

execOpEQUAL :: Insts -> Stack -> Either String Value
execOpEQUAL insts ((IntVM i1):(IntVM i2):stack) = exec insts ((BoolVM $ i1 == i2) : stack)
execOpEQUAL insts ((BoolVM b1):(BoolVM b2):stack) = exec insts ((BoolVM $ b1 == b2) : stack)
execOpEQUAL _ _ = Left "EQUAL need two arguments"

execOpLESS :: Insts -> Stack -> Either String Value
execOpLESS insts ((IntVM i1):(IntVM i2):stack) = exec insts ((BoolVM $ i1 < i2) : stack)
execOpLESS insts ((BoolVM b1):(BoolVM b2):stack) = exec insts ((BoolVM $ b1 < b2) : stack)
execOpLESS _ _ = Left "LESS need two arguments"

remInst :: Int -> Insts -> Insts
remInst _ [] = []
remInst i (x:xs) | i > 0 = remInst (i - 1) xs
                 | otherwise = x : xs

execJumpIfFalse :: Int -> Insts -> Stack -> Either String Value
execJumpIfFalse i insts ((BoolVM False):stack) = exec (remInst i insts) stack
execJumpIfFalse i insts ((IntVM 0):stack) = exec (remInst i insts) stack
execJumpIfFalse _ insts stack = exec insts stack

exec :: Insts -> Stack -> Either String Value
exec ((CallOp ADD):insts) stack = execOpADD insts stack
exec ((CallOp SUB):insts) stack = execOpSUB insts stack
exec ((CallOp MUL):insts) stack = execOpMUL insts stack
exec ((CallOp DIV):insts) stack = execOpDIV insts stack
exec ((CallOp MOD):insts) stack = execOpMOD insts stack
exec ((CallOp EQUAL):insts) stack = execOpEQUAL insts stack
exec ((CallOp LESS):insts) stack = execOpLESS insts stack
exec ((Push value):insts) stack = exec insts (value : stack)
exec (Ret:_) (value:_) = Right value
exec ((JUMPIFFALSE i):insts) stack = execJumpIfFalse i insts stack
exec ((JUMP i):insts) stack = exec (remInst i insts) stack
exec _ _ = Left "no value to return"

parseVMCall :: String -> Maybe Instruction
parseVMCall "ADD" = Just $ CallOp ADD
parseVMCall "SUB" = Just $ CallOp SUB
parseVMCall "MUL" = Just $ CallOp MUL
parseVMCall "DIV" = Just $ CallOp DIV
parseVMCall "MOD" = Just $ CallOp MOD
parseVMCall "EQUAL" = Just $ CallOp EQUAL
parseVMCall "LESS" = Just $ CallOp LESS
parseVMCall _        = Nothing

parseVMPush :: String -> Maybe Instruction
parseVMPush "True" = Just $ Push $ BoolVM True
parseVMPush "False" = Just $ Push $ BoolVM False
parseVMPush ('-':xs) | isStringNumber xs = Just $ Push $ IntVM $ ((read xs) :: Int) * (-1)
parseVMPush str      | isStringNumber str = Just $ Push $ IntVM $ ((read str) :: Int)
parseVMPush _        = Nothing

parseJump :: String -> Maybe Int
parseJump str | isStringNumber str = Just $ ((read str) :: Int)
parseJump _   = Nothing

parseVM :: String -> Insts
parseVM str = let (first, next) = firstWord str
                  (second, rest) = firstWord next in
              case first of
                "CALL" -> case parseVMCall second of
                            Just inst -> inst : (parseVM rest)
                            Nothing -> parseVM rest
                "PUSH" -> case parseVMPush second of
                            Just inst -> inst : (parseVM rest)
                            Nothing -> parseVM rest
                "RET" -> Ret : parseVM rest
                "JUMPIFFALSE" -> case parseJump second of
                            Just i -> (JUMPIFFALSE i) : (parseVM rest)
                            Nothing -> parseVM rest
                "JUMP" -> case parseJump second of
                            Just i -> (JUMP i) : (parseVM rest)
                            Nothing -> parseVM rest
                _ -> []

startVM :: String -> IO ()
startVM file = do
    result <- try (readFile file) :: IO (Either SomeException String)
    case result of
        Left e -> (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
        Right content -> case exec (parseVM content) [] of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right value -> case value of
                                            (IntVM i) -> putStrLn $ show i
                                            (BoolVM b) -> putStrLn $ show b
