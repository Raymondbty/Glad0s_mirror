{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Funcs.hs
-}

module Funcs (factorial, fact, equal, lower,
plus, minus, mul, myDiv, myMod, greater, myLeq, myGeq,
notequal) where

import Print
import Types

factorial :: Int -> Either String Int
factorial n | n < 0 = Left "n must be > or = to 0"
            | n > 1 = case factorial $ n - 1 of
                        Left err -> Left err
                        Right i -> Right $ n * i
            | otherwise = Right 1

fact :: Either String Ast -> Either String Ast
fact (Left err) = Left err
fact (Right ast) = case ast of
                (Call _ [IntLiteral n]) ->
                    case factorial n of
                        Left err -> Left err
                        Right i -> Right $ IntLiteral i
                _ -> Left $ "require one argument"

myLeq :: Either String Ast -> Either String (Ast, [Env])
myLeq (Left err) = Left err
myLeq (Right ast) = case ast of
    (Call _ [IntLiteral x, IntLiteral y]) ->
        Right $ (BoolLiteral $ x <= y, [])
    _ -> Left $ wrongArguments ast

myGeq :: Either String Ast -> Either String (Ast, [Env])
myGeq (Left err) = Left err
myGeq (Right ast) = case ast of
    (Call _ [IntLiteral x, IntLiteral y]) ->
        Right $ (BoolLiteral $ x >= y, [])
    _ -> Left $ wrongArguments ast

equal :: Either String Ast -> Either String (Ast, [Env])
equal (Left err) = Left err
equal (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (BoolLiteral $ x == y, [])
                (Call _ [StringLiteral str1, StringLiteral str2]) ->
                    Right $ (BoolLiteral $ str1 == str2, [])
                _ -> Left $ wrongArguments ast

notequal :: Either String Ast -> Either String (Ast, [Env])
notequal (Left err) = Left err
notequal (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (BoolLiteral $ x /= y, [])
                (Call _ [StringLiteral str1, StringLiteral str2]) ->
                    Right $ (BoolLiteral $ str1 /= str2, [])
                _ -> Left $ wrongArguments ast


lower :: Either String Ast -> Either String (Ast, [Env])
lower (Left err) = Left err
lower (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (BoolLiteral $ x < y, [])
                _ -> Left $ wrongArguments ast

greater :: Either String Ast -> Either String (Ast, [Env])
greater (Left err) = Left err
greater (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (BoolLiteral $ x < y, [])
                _ -> Left $ wrongArguments ast

plus :: Either String Ast -> Either String (Ast, [Env])
plus (Left err) = Left err
plus (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right (IntLiteral $ x + y, [])
                _ -> Left $ wrongArguments ast

minus :: Either String Ast -> Either String (Ast, [Env])
minus (Left err) = Left err
minus (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (IntLiteral $ x - y, [])
                _ -> Left $ wrongArguments ast

mul :: Either String Ast -> Either String (Ast, [Env])
mul (Left err) = Left err
mul (Right ast) = case ast of
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (IntLiteral $ x * y, [])
                _ -> Left $ wrongArguments ast

myDiv :: Either String Ast -> Either String (Ast, [Env])
myDiv (Left err) = Left err
myDiv (Right ast) = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) ->
                    Right $ (IntLiteral 0, [])
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (IntLiteral $ x `div` y, [])
                _ -> Left $ wrongArguments ast

myMod :: Either String Ast -> Either String (Ast, [Env])
myMod (Left err) = Left err
myMod (Right ast) = case ast of
                (Call _ [IntLiteral _, IntLiteral 0]) ->
                    Right $ (IntLiteral 0, [])
                (Call _ [IntLiteral x, IntLiteral y]) ->
                    Right $ (IntLiteral $ x `mod` y, [])
                _ -> Left $ wrongArguments ast
