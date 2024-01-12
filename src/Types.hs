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
         | Lambda [String] Ast
         deriving Show

data Env = Var String Ast
         deriving Show
