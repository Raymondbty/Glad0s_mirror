{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Print.hs
-}

module Print (prettyPrint) where

import Types

showPrint :: Ast -> String
showPrint (IntLiteral i) = show i
showPrint (StringLiteral str) = str
showPrint (BoolLiteral bool) = show bool
showPrint ast = show ast

prettyPrint :: [Ast] -> String
prettyPrint [] = []
prettyPrint (x:xs) = (showPrint x) ++ (prettyPrint xs)
