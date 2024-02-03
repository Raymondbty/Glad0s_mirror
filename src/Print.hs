{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Print.hs
-}

module Print (prettyPrint, prettyPrintString, wrongArguments) where

import Types

prettyPrintList :: [Ast] -> String
prettyPrintList [] = []
prettyPrintList [x] = prettyPrint x
prettyPrintList (x:xs) = (prettyPrint x) ++ ", " ++ (prettyPrintList xs)

prettyPrint :: Ast -> String
prettyPrint (IntLiteral i) = show i
prettyPrint (StringLiteral str) = show str
prettyPrint (Call symbol list) =
    symbol ++ "(" ++(prettyPrintList list) ++ ");"
prettyPrint (If cond _ _) = "if (" ++ (prettyPrint cond) ++ ")"
prettyPrint (BoolLiteral b) = show b
prettyPrint (Symbol str) = str
prettyPrint (Lambda _ _) = "#<procedure>"
prettyPrint (Print []) = []
prettyPrint (Print (x:xs)) = (prettyPrint x) ++ (prettyPrint (Print xs))
prettyPrint ast = show ast

prettyPrintString :: Ast -> String
prettyPrintString (StringLiteral str) = str
prettyPrintString (Print []) = []
prettyPrintString (Print (x:xs)) =
    (prettyPrintString x) ++ (prettyPrintString (Print xs))
prettyPrintString ast = prettyPrint ast

wrongArguments :: Ast -> String
wrongArguments _ = "require two arguments"
