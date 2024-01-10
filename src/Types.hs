{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Types.hs
-}

module Types (Ast(..), Env(..)) where

data Ast = Define String Ast
         | Call String [Ast]
         | IntLiteral Int
         | StringLiteral String
         | Symbol String
         | BoolLiteral Bool
         deriving Show

data Env = String Ast
         deriving Show
