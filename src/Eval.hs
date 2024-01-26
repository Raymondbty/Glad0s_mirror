{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Eval (evalASTIfCond, evalAST) where

import Funcs
import Types

evalASTCallArgs :: [Ast] -> [Env] -> Either String [Ast]
evalASTCallArgs [] _ = Right []
evalASTCallArgs (x:xs) env = case evalAST x env of
                                Left err -> Left err
                                Right (ast, _) -> case evalASTCallArgs xs env of
                                    Left err -> Left err
                                    Right asts -> Right (ast : asts)

evalASTCall :: Ast -> [Env] -> Either String Ast
evalASTCall (Call func args) env = case evalASTCallArgs args env of
                                    Left err -> Left err
                                    Right ast -> Right (Call func ast)
evalASTCall ast _ = Right ast

evalASTIfCond :: Ast -> [Env] -> Either String (Ast, [Env])
evalASTIfCond (Call _ [cond, trueBranch, falseBranch]) env =
    case evalAST cond env of
        Right (BoolLiteral True, _) -> evalAST trueBranch env
        Right (BoolLiteral False, _) -> evalAST falseBranch env
        _ -> Left "require three arguments"
evalASTIfCond _ _ = Left "require three arguments"

getASTInEnv :: String -> [Env] -> Maybe Ast
getASTInEnv _ [] = Nothing
getASTInEnv str ((Var key ast):xs) = if str == key
                                     then Just ast
                                     else getASTInEnv str xs
getASTInEnv str ((FuncVar key asts):xs) = if str == key
                                     then Just $ FuncRes asts
                                     else getASTInEnv str xs

evalFunction :: [Ast] -> [Env] -> Either String ([Ast], [Env])
evalFunction [] env = Right ([], env)
evalFunction (x:xs) env = case evalAST x env of
    Left err -> Left err
    Right ((Print ast), env1) -> case evalFunction xs env1 of
        Left err -> Left err
        Right (asts, env2) -> Right ((Print ast) : asts, env2)
    Right (_, env1) -> case evalFunction xs env1 of
        Left err -> Left err
        Right (asts, env2) -> Right (asts, env2)

evalAST :: Ast -> [Env] -> Either String (Ast, [Env])
evalAST ast env = case ast of
                (Define name ast1) -> Right (ast1, (Var name ast1) : env)
                (Func name args) -> Right (ast, (FuncVar name args) : env)
                (FuncRes asts) -> case evalFunction asts env of
                    Left err -> Left err
                    Right (asts1, _) -> Right (FuncRes asts1, env)
                (Call "add" _) -> plus $ evalASTCall ast env
                (Call "mul" _) -> mul $ evalASTCall ast env
                (Call "sub" _) -> minus $ evalASTCall ast env
                (Call "print" [ast1]) -> case evalAST ast1 env of
                    Left err -> Left err
                    Right (ast2, _) -> Right (Print ast2, env)
                (Call "print" _) -> Left "print takes only one argument"
                (Call func args) -> case getASTInEnv func env of
                    Just ast1 -> evalAST ast1 env
                    Nothing -> Left $ "function " ++ func ++ " not found"
                (Symbol str) -> case getASTInEnv str env of
                    Just ast1 -> evalAST ast1 env
                    Nothing -> Left $ "variable " ++ str ++ " not found"
                _ -> Right (ast, env)
