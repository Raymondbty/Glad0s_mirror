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
getASTInEnv str ((FuncVar key args asts):xs) = if str == key
                                     then Just $ FuncArgs args asts
                                     else getASTInEnv str xs

evalFuncCallArgs :: [String] -> [Ast] -> Maybe [Env]
evalFuncCallArgs [] [] = Just []
evalFuncCallArgs [] _ = Nothing
evalFuncCallArgs _ [] = Nothing
evalFuncCallArgs (x:xs) ((Symbol s):as) | x == s = evalFuncCallArgs xs as
evalFuncCallArgs (x:xs) (a:as) = case evalFuncCallArgs xs as of
                                    Just env -> Just $ (Var x a) : env
                                    Nothing -> Nothing

getFuncASTInEnv :: String -> [Ast] -> [Env] -> Either String (Ast, [Env])
getFuncASTInEnv func args env =
    case getASTInEnv func env of
        Just (FuncArgs astsArgs asts) -> case evalFuncCallArgs astsArgs args of
            Just env1 -> evalAST (FuncRes asts) (env1 ++ env)
            Nothing -> Left $ "function " ++ func ++ " wrong arguments"
        _ -> Left $ "function " ++ func ++ " not found"

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

evalPrint :: [Ast] -> [Env] -> Either String [Ast]
evalPrint [] _ = Right []
evalPrint (x:xs) env =
    case evalAST x env of
        Left err -> Left err
        Right (ast, _) ->
            case evalPrint xs env of
                            Left err -> Left err
                            Right asts -> Right $ ast : asts

evalAST :: Ast -> [Env] -> Either String (Ast, [Env])
evalAST ast env = case ast of
                (Define name ast1) -> Right (ast1, (Var name ast1) : env)
                (Func name args asts) -> Right (ast, (FuncVar name args asts) : env)
                (FuncRes asts) -> case evalFunction asts env of
                    Left err -> Left err
                    Right (asts1, _) -> Right (FuncRes asts1, env)
                (Call "add" _) -> plus $ evalASTCall ast env
                (Call "mul" _) -> mul $ evalASTCall ast env
                (Call "div" _) -> myDiv $ evalASTCall ast env
                (Call "mod" _) -> myMod $ evalASTCall ast env
                (Call "sub" _) -> minus $ evalASTCall ast env
                (Call "equal" _) -> equal $ evalASTCall ast env
                (Call "if" _) -> evalASTIfCond ast env
                (Call "print" asts) -> case evalPrint asts env of
                    Left err -> Left err
                    Right asts1 -> Right (Print asts1, env)
                (Call func args) -> getFuncASTInEnv func args env
                (Symbol str) -> case getASTInEnv str env of
                    Just ast1 -> evalAST ast1 env
                    Nothing -> Left $ "variable " ++ str ++ " not found"
                _ -> Right (ast, env)