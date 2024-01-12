{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Print.hs
-}

module Print (prettyPrint, wrongArguments) where

import Types

prettyPrintList :: [Ast] -> String
prettyPrintList [] = ""
prettyPrintList (x:xs) = " " ++ (prettyPrint x) ++ (prettyPrintList xs)

prettyPrint :: Ast -> String
prettyPrint (IntLiteral i) = show i
prettyPrint (StringLiteral str) = show str
prettyPrint (Call symbol list) = "(" ++ symbol ++ (prettyPrintList list) ++ ")"
prettyPrint (BoolLiteral b) = show b
prettyPrint (Symbol str) = show str
prettyPrint ast = show ast

wrongArguments :: Ast -> String
wrongArguments _ = "require two arguments"
