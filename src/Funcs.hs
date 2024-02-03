{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Funcs.hs
-}

module Funcs (myLeq
            , myGeq
            , equal
            , notequal
            , lower
            , greater
            , add
            , sub
            , mul
            , myDiv
            , myMod) where

import Types

myLeq :: [Ast] -> Either String Ast
myLeq [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 <= i2
myLeq _ = Left $ "leq: need two integers"

myGeq :: [Ast] -> Either String Ast
myGeq [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 >= i2
myGeq _ = Left $ "geq: need two integers"

equal :: [Ast] -> Either String Ast
equal [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 == i2
equal [StringLiteral s1, StringLiteral s2] = Right $ BoolLiteral $ s1 == s2
equal _ = Left $ "equal: need two integers or two strings"

notequal :: [Ast] -> Either String Ast
notequal [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 /= i2
notequal [StringLiteral s1, StringLiteral s2] = Right $ BoolLiteral $ s1 /= s2
notequal _ = Left $ "ne: need two integers or two strings"

lower :: [Ast] -> Either String Ast
lower [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 < i2
lower _ = Left $ "lower: need two integers"

greater :: [Ast] -> Either String Ast
greater [IntLiteral i1, IntLiteral i2] = Right $ BoolLiteral $ i1 > i2
greater _ = Left $ "greater: need two integers"

add :: [Ast] -> Either String Ast
add [IntLiteral i1, IntLiteral i2] = Right $ IntLiteral $ i1 + i2
add _ = Left $ "add: need two integers"

sub :: [Ast] -> Either String Ast
sub [IntLiteral i1, IntLiteral i2] = Right $ IntLiteral $ i1 - i2
sub _ = Left $ "sub: need two integers"

mul :: [Ast] -> Either String Ast
mul [IntLiteral i1, IntLiteral i2] = Right $ IntLiteral $ i1 * i2
mul _ = Left $ "mul: need two integers"

myDiv :: [Ast] -> Either String Ast
myDiv [IntLiteral _, IntLiteral 0] = Right $ IntLiteral 0
myDiv [IntLiteral i1, IntLiteral i2] = Right $ IntLiteral $ i1 `div` i2
myDiv _ = Left $ "div: need two integers"

myMod :: [Ast] -> Either String Ast
myMod [IntLiteral _, IntLiteral 0] = Right $ IntLiteral 0
myMod [IntLiteral i1, IntLiteral i2] = Right $ IntLiteral $ i1 `mod` i2
myMod _ = Left $ "mod: need two integers"
