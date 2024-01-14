{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Compiler.hs
-}

module Compiler (compile) where

import Control.Exception
import System.Exit
import Types

convAst :: Ast -> String
convAst (Call "+" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL ADD\n"
convAst (Call "-" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL SUB\n"
convAst (Call "*" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MUL\n"
convAst (Call "/" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL DIV\n"
convAst (Call "div" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL DIV\n"
convAst (Call "%" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MOD\n"
convAst (Call "mod" (ast1:ast2:_)) = (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MOD\n"
convAst (IntLiteral i) = "PUSH " ++ (show i) ++ "\n"
convAst _ = []

loopAst :: [Ast] -> String
loopAst [] = []
loopAst (x:xs) = (convAst x) ++ (loopAst xs)

compile :: [Ast] -> String -> IO ()
compile [] _ = return ()
compile (x:_) path = do
    result <- try (writeFile path ((loopAst [x]) ++ "RET\n")) :: IO (Either SomeException ())
    case result of
        Left e -> (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
        Right () -> return ()
