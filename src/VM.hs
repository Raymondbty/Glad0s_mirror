{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- VM.hs
-}

module VM (startVM) where

import Control.Exception
import System.Exit
import Types

execOpADD :: Stack -> Either String Value
execOpADD ((IntVM i1):(IntVM i2):_) = Right $ IntVM $ i1 + i2
execOpADD _ = Left "ADD need two integers"

execOpSUB :: Stack -> Either String Value
execOpSUB ((IntVM i1):(IntVM i2):_) = Right $ IntVM $ i1 - i2
execOpSUB _ = Left "SUB need two integers"

execOpMUL :: Stack -> Either String Value
execOpMUL ((IntVM i1):(IntVM i2):_) = Right $ IntVM $ i1 * i2
execOpMUL _ = Left "MUL need two integers"

execOpDIV :: Stack -> Either String Value
execOpDIV ((IntVM _):(IntVM 0):_) = Left "division by 0"
execOpDIV ((IntVM i1):(IntVM i2):_) = Right $ IntVM $ i1 `div` i2
execOpDIV _ = Left "DIV need two integers"

execOpMOD :: Stack -> Either String Value
execOpMOD ((IntVM _):(IntVM 0):_) = Left "modulo by 0"
execOpMOD ((IntVM i1):(IntVM i2):_) = Right $ IntVM $ i1 `mod` i2
execOpMOD _ = Left "MOD need two integers"

execOpEQUAL :: Stack -> Either String Value
execOpEQUAL ((IntVM i1):(IntVM i2):_) = Right $ BoolVM $ i1 == i2
execOpEQUAL ((BoolVM b1):(BoolVM b2):_) = Right $ BoolVM $ b1 == b2
execOpEQUAL _ = Left "EQUAL need two arguments"

execOpLESS :: Stack -> Either String Value
execOpLESS ((IntVM i1):(IntVM i2):_) = Right $ BoolVM $ i1 < i2
execOpLESS ((BoolVM b1):(BoolVM b2):_) = Right $ BoolVM $ b1 < b2
execOpLESS _ = Left "LESS need two arguments"

exec :: Insts -> Stack -> Either String Value
exec ((CallOp ADD):_) stack = execOpADD stack
exec ((CallOp SUB):_) stack = execOpSUB stack
exec ((CallOp MUL):_) stack = execOpMUL stack
exec ((CallOp DIV):_) stack = execOpDIV stack
exec ((CallOp MOD):_) stack = execOpMOD stack
exec ((CallOp EQUAL):_) stack = execOpEQUAL stack
exec ((CallOp LESS):_) stack = execOpLESS stack
exec ((Push value):insts) stack = exec insts (value : stack)
exec (Ret:_) (value:_) = Right value
exec (Ret:_) [] = Left "no value to return"
exec [] _ = Left "no instruction"

parseVM :: String -> Insts
parseVM _ = []

startVM :: String -> IO ()
startVM file = do
    result <- try (readFile file) :: IO (Either SomeException String)
    case result of
        Left e -> (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
        Right content -> case exec (parseVM content) [] of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right value -> putStrLn $ show $ value
