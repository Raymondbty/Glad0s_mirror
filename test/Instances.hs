{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Instances.hs
-}

module Instances (SExpr, Ast) where

import Types
import Glados

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
