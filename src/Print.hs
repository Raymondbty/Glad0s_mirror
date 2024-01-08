{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Print.hs
-}

module Print (prettyPrint, wrongArguments, noMatchingFunction) where

import Ast

prettyPrintList :: [Ast] -> String
prettyPrintList [] = ""
prettyPrintList (x:xs) = " " ++ (prettyPrint x) ++ (prettyPrintList xs)

prettyPrint :: Ast -> String
prettyPrint (IntLiteral i) = show i
prettyPrint (StringLiteral str) = show str
prettyPrint (Call symbol list) = "(" ++ symbol ++ (prettyPrintList list) ++ ")"
prettyPrint (BoolLiteral b) = show b
prettyPrint ast = show ast

wrongArguments :: Ast -> String
wrongArguments ast = case ast of
                        (Call f _) -> "function '" ++ f ++ "' require two arguments " ++ (prettyPrint ast)
                        _ -> ""

noMatchingFunction :: Ast -> String
noMatchingFunction ast = case ast of
                        (Call f _) -> "no matching function '" ++ f ++ "' " ++ (prettyPrint ast)
                        _ -> ""
