{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Funcs.hs
-}

module Funcs (plus, minus, mul, myDiv, myMod) where

import Ast
import Print

plus :: Either String Ast -> Either String Ast
plus (Left err) = Left err
plus (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x + y
                _ -> Left $ wrongArguments ast

minus :: Either String Ast -> Either String Ast
minus (Left err) = Left err
minus (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x - y
                _ -> Left $ wrongArguments ast

mul :: Either String Ast -> Either String Ast
mul (Left err) = Left err
mul (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x * y
                _ -> Left $ wrongArguments ast

myDiv :: Either String Ast -> Either String Ast
myDiv (Left err) = Left err
myDiv (Right ast) = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) -> Right $ IntLiteral 0
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x `div` y
                _ -> Left $ wrongArguments ast

myMod :: Either String Ast -> Either String Ast
myMod (Left err) = Left err
myMod (Right ast) = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) -> Right $ IntLiteral 0
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x `mod` y
                _ -> Left $ wrongArguments ast
