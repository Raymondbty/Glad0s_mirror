{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Compiler.hs
-}

module Compiler (compile, myLength, convAst, loopAst) where

import Control.Exception
import System.Exit
import Types

myLength :: String -> Int
myLength [] = 0
myLength ('\n':xs) = 1 + (myLength xs)
myLength (_:xs) = (myLength xs)

convAst :: Ast -> String
convAst (Call "+" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL ADD\n"
convAst (Call "-" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL SUB\n"
convAst (Call "*" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MUL\n"
convAst (Call "/" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL DIV\n"
convAst (Call "div" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL DIV\n"
convAst (Call "%" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MOD\n"
convAst (Call "mod" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL MOD\n"
convAst (Call "?" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL EQUAL\n"
convAst (Call "<" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ "CALL LESS\n"
convAst (Call "if" (ast1:ast2:ast3:_)) =
  let cond = loopAst [ast1]
      trueCond =
        loopAst [ast2] ++ "JUMP " ++ (show $ myLength falseCond) ++ "\n"
      falseCond = loopAst [ast3] in
    cond ++ "JUMPIFFALSE " ++ (show $ myLength trueCond) ++ "\n" ++ trueCond
    ++ falseCond
convAst (IntLiteral i) = "PUSH " ++ (show i) ++ "\n"
convAst (BoolLiteral b) = "PUSH " ++ (show b) ++ "\n"
convAst _ = []

loopAst :: [Ast] -> String
loopAst [] = []
loopAst (x:xs) = (convAst x) ++ (loopAst xs)

compile :: [Ast] -> String -> IO ()
compile [] _ = return ()
compile (x:_) path = do
  result <- try (writeFile path ((loopAst [x]) ++ "RET\n")) ::
    IO (Either SomeException ())
  case result of
    Left e ->
      (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
    Right () -> return ()
