{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Ast (Ast(..)) where

data Ast = Define String Ast
         | Call String [Ast]
         | IntLiteral Int
         | StringLiteral String
         | Symbol String
         deriving Show
