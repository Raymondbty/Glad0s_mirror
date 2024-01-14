{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Types.hs
-}

module Types (Ast(..), Env(..), Value(..), Operator(..), Instruction(..), Stack, Insts) where

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

data Value = IntVM Int
           | BoolVM Bool
           deriving Show

data Operator = ADD
              | SUB
              | MUL
              | DIV
              | MOD
              | EQUAL
              | LESS
              deriving Show

data Instruction = Push Value
                 | CallOp Operator
                 | Ret
                 deriving Show

type Stack = [Value]

type Insts = [Instruction]