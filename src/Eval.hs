{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Eval (evalAST) where

import Ast
import Funcs
import Print

evalASTCallArgs :: [Ast] -> Either String [Ast]
evalASTCallArgs [] = Right []
evalASTCallArgs (x:xs) = case evalAST x of
                            Left err -> Left err
                            Right ast -> case evalASTCallArgs xs of
                                            Left err -> Left err
                                            Right asts -> Right (ast : asts)

evalASTCall :: Ast -> Either String Ast
evalASTCall (Call func args) = case evalASTCallArgs args of
                                Left err -> Left err
                                Right ast -> Right (Call func ast)
evalASTCall ast = Right ast

evalASTIfCond :: Ast -> Either String Ast
evalASTIfCond ast = case ast of
                        (Call _ [cond, trueBranch, falseBranch]) -> case evalAST cond of
                                                                        Right (BoolLiteral True) -> evalAST trueBranch
                                                                        Right (BoolLiteral False) -> evalAST falseBranch
                                                                        _ -> Left $ wrongArgumentsIfCond ast
                        _ -> Left $ wrongArgumentsIfCond ast

evalAST :: Ast -> Either String Ast
evalAST ast = case ast of
                (Call "if" _) -> evalASTIfCond ast
                (Call "eq?" _) -> equal $ evalASTCall ast
                (Call "<" _) -> lower $ evalASTCall ast
                (Call "+" _) -> plus $ evalASTCall ast
                (Call "-" _) -> minus $ evalASTCall ast
                (Call "*" _) -> mul $ evalASTCall ast
                (Call "/" _) -> myDiv $ evalASTCall ast
                (Call "div" _) -> myDiv $ evalASTCall ast
                (Call "%" _) -> myMod $ evalASTCall ast
                (Call "mod" _) -> myMod $ evalASTCall ast
                (Call _ _) -> Left $ noMatchingFunction ast
                _ -> Right ast
