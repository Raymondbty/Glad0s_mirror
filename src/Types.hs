{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Types.hs
-}

module Types (Ast(..), Env(..), Value(..), Operator(..), Instruction(..), Stack, Insts, Parser(..)) where

import Control.Applicative

data Ast = Define String Ast
         | Func String [String] [Ast]
         | Call String [Ast]
         | IntLiteral Int
         | StringLiteral String
         | Symbol String
         | BoolLiteral Bool
         | If Ast [Ast] [Ast]
         | While Ast [Ast]
         | Void
         | Return Ast
         deriving Show

data Env = Var String Ast
         | FuncVar String [String] [Ast]
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
                 | JumpIfFalse Int
                 | Jump Int
                 deriving Show

type Stack = [Value]

type Insts = [Instruction]

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str ->
        case runParser parser str of
            Just (res, rest) -> Just (fct res, rest)
            Nothing -> Nothing

instance Applicative Parser where
    pure res = Parser $ \rest -> Just (res, rest)
    parserf <*> parser = Parser $ \str ->
        case runParser parserf str of
            Just (fct, rest) ->
                case runParser parser rest of
                    Just (res, rest1) -> Just (fct res, rest1)
                    Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    parser1 <|> parser2 = Parser $ \str ->
        case runParser parser1 str of
            Just (res, rest) -> Just (res, rest)
            Nothing -> runParser parser2 str

instance Monad Parser where
    return = pure
    parser >>= fct = Parser $ \str ->
        case runParser parser str of
            Just (res, rest) -> runParser (fct res) rest
            Nothing -> Nothing
