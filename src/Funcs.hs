{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Funcs.hs
-}

module Funcs (plus, minus, mul, myDiv, myMod) where

import Ast
import Print

plus :: Ast -> Either String Ast
plus ast = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x + y
                _ -> Left $ wrongArguments ast

minus :: Ast -> Either String Ast
minus ast = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x - y
                _ -> Left $ wrongArguments ast

mul :: Ast -> Either String Ast
mul ast = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x * y
                _ -> Left $ wrongArguments ast

myDiv :: Ast -> Either String Ast
myDiv ast = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) -> Right $ IntLiteral 0
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x `div` y
                _ -> Left $ wrongArguments ast

myMod :: Ast -> Either String Ast
myMod ast = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) -> Right $ IntLiteral 0
                (Call _ [IntLiteral x, IntLiteral y]) -> Right $ IntLiteral $ x `mod` y
                _ -> Left $ wrongArguments ast
