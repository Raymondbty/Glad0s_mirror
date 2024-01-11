{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Eval (evalASTIfCond, evalAST) where

import Funcs
import Print
import Types

evalASTCallArgs :: [Ast] -> [Env] -> Either String [Ast]
evalASTCallArgs [] _ = Right []
evalASTCallArgs (x:xs) env = case evalAST x env of
                                Left err -> Left err
                                Right ast -> case evalASTCallArgs xs env of
                                                Left err -> Left err
                                                Right asts -> Right (ast : asts)

evalASTCall :: Ast -> [Env] -> Either String Ast
evalASTCall (Call func args) env = case evalASTCallArgs args env of
                                    Left err -> Left err
                                    Right ast -> Right (Call func ast)
evalASTCall ast _ = Right ast

evalASTIfCond :: Ast -> [Env] -> Either String Ast
evalASTIfCond ast env = case ast of
                            (Call _ [cond, trueBranch, falseBranch]) -> case evalAST cond env of
                                                                        Right (BoolLiteral True) -> evalAST trueBranch env
                                                                        Right (BoolLiteral False) -> evalAST falseBranch env
                                                                        _ -> Left $ wrongArgumentsIfCond ast
                            _ -> Left $ wrongArgumentsIfCond ast

getASTInEnv :: String -> [Env] -> Maybe Ast
getASTInEnv _ [] = Nothing
getASTInEnv str ((Var key ast):xs) = if str == key
                                     then Just ast
                                     else getASTInEnv str xs

lookSymbolInEnv :: String -> [Env] -> Either String Ast
lookSymbolInEnv str env = case getASTInEnv str env of
                                Just ast -> evalAST ast env
                                Nothing -> Left $ "variable " ++ str ++ " not found"

evalAST :: Ast -> [Env] -> Either String Ast
evalAST ast env = case ast of
                (Call "if" _) -> evalASTIfCond ast env
                (Call "eq?" _) -> equal $ evalASTCall ast env
                (Call "<" _) -> lower $ evalASTCall ast env
                (Call "+" _) -> plus $ evalASTCall ast env
                (Call "-" _) -> minus $ evalASTCall ast env
                (Call "*" _) -> mul $ evalASTCall ast env
                (Call "/" _) -> myDiv $ evalASTCall ast env
                (Call "div" _) -> myDiv $ evalASTCall ast env
                (Call "%" _) -> myMod $ evalASTCall ast env
                (Call "mod" _) -> myMod $ evalASTCall ast env
                (Call _ _) -> Left $ noMatchingFunction ast
                (Symbol str) -> lookSymbolInEnv str env
                _ -> Right ast
