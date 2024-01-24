{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Types.hs
-}

module Types (Ast(..), Env(..), Value(..), Operator(..), Instruction(..), Stack, Insts) where

data Ast = Define String Ast
         | Func String [Ast]
         | Call String [Ast]
         | Call2 String [String] [Ast]
         | IntLiteral Int
         | StringLiteral String
         | Symbol String
         | BoolLiteral Bool
         | Lambda [String] Ast
         | Print Ast
         | FuncRes [Ast]
         deriving Show

data Env = Var String Ast
         | FuncVar String [Ast]
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
              | FACT
              deriving Show

data Instruction = Push Value
                 | CallOp Operator
                 | Ret
                 | JumpIfFalse Int
                 | Jump Int
                 deriving Show

type Stack = [Value]

type Insts = [Instruction]
