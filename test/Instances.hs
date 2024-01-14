{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Instances.hs
-}

module Instances (SExpr, Ast) where

import Types
import Glados

instance Eq Operator where
    ADD == ADD = True
    SUB == SUB = True
    MUL == MUL = True
    DIV == DIV = True
    MOD == MOD = True
    EQUAL == EQUAL = True
    LESS == LESS = True
    _ == _ = False

instance Eq SExpr where
  (SList a) == (SList b) = a == b
  (SSymbol a) == (SSymbol b) = a == b
  (SInt a) == (SInt b) = a == b
  _ == _ = False

instance Eq Ast where
  IntLiteral a == IntLiteral b = a == b
  StringLiteral a == StringLiteral b = a == b
  Define sym1 expr1 == Define sym2 expr2 = sym1 == sym2 && expr1 == expr2
  _ == _ = False

instance Eq Value where
    (IntVM x) == (IntVM y) = x == y
    (BoolVM x) == (BoolVM y) = x == y

instance Eq Instruction where
    (Push v1) == (Push v2) = v1 == v2
    (CallOp op1) == (CallOp op2) = op1 == op2
    Ret == Ret = True
    (JUMPIFFALSE i1) == (JUMPIFFALSE i2) = i1 == i2
    (JUMP i1) == (JUMP i2) = i1 == i2
    _ == _ = False

