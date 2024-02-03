{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Eval (evalASTIfCond, evalAST) where

import Funcs
import Types

evalASTCallArgs :: Int -> [Ast] -> [Env] -> Either String [Ast]
evalASTCallArgs _ [] _ = Right []
evalASTCallArgs i (x:xs) env = case evalAST i x env of
                                Left err -> Left err
                                Right (ast, _) -> case evalASTCallArgs i xs env of
                                    Left err -> Left err
                                    Right asts -> Right (ast : asts)

evalASTCall :: Int -> Ast -> [Env] -> Either String Ast
evalASTCall i (Call func args) env = case evalASTCallArgs i args env of
                                    Left err -> Left err
                                    Right ast -> Right (Call func ast)
evalASTCall _ ast _ = Right ast

evalASTIfCond :: Int -> Ast -> [Env] -> Either String (Ast, [Env])
evalASTIfCond i (If cond trueBranch falseBranch) env =
    case evalAST i cond env of
        Right (BoolLiteral True, _) -> evalIf trueBranch
        Right (BoolLiteral False, _) -> evalIf falseBranch
        Right (IntLiteral 0, _) -> evalIf falseBranch
        Right (IntLiteral _, _) -> evalIf trueBranch
        _ -> Left "if condition not a boolean"
    where
        evalIf branch =
            case evalFunction i branch env of
                Right (asts, env1) -> evalAST i (IfRes asts) (env1 ++ env)
                Left err -> Left err
evalASTIfCond _ _ _ = Left "require three arguments"

evalASTWhileCond :: Int -> Ast -> [Env] -> Either String ([Ast], [Env])
evalASTWhileCond i (While cond branch) env =
    case evalAST i cond env of
        Right (BoolLiteral True, _) -> evalWhile
        Right (BoolLiteral False, _) -> Right ([], env)
        Right (IntLiteral 0, _) -> Right ([], env)
        Right (IntLiteral _, _) -> evalWhile
        _ -> Left "while condition not a boolean"
    where
        evalWhile =
            case evalFunction i branch env of
                Right (asts, env1) ->
                    case evalASTWhileCond i (While cond branch) env1 of
                        Right (asts1, env2) -> Right (asts ++ asts1, env2)
                        Left err -> Left err
                Left err -> Left err
evalASTWhileCond _ _ _ = Left "require two arguments"

getASTInEnv :: String -> [Env] -> Maybe (Ast, [Env])
getASTInEnv _ [] = Nothing
getASTInEnv str ((Var key ast):xs) = if str == key
                                     then Just (ast, xs)
                                     else getASTInEnv str xs
getASTInEnv str ((FuncVar key args asts):xs) = if str == key
                                     then Just (FuncArgs args asts, xs)
                                     else getASTInEnv str xs

evalFuncCallArgs :: [String] -> [Ast] -> Maybe [Env]
evalFuncCallArgs [] [] = Just []
evalFuncCallArgs [] _ = Nothing
evalFuncCallArgs _ [] = Nothing
evalFuncCallArgs (x:xs) ((Symbol s):as) | x == s = evalFuncCallArgs xs as
evalFuncCallArgs (x:xs) (a:as) = case evalFuncCallArgs xs as of
                                    Just env -> Just $ (Var x a) : env
                                    Nothing -> Nothing

getFuncASTInEnv :: Int -> String -> [Ast] -> [Env] -> Either String (Ast, [Env])
getFuncASTInEnv i func args env =
    case getASTInEnv func env of
        Just (FuncArgs astsArgs asts, _) -> case evalFuncCallArgs astsArgs args of
            Just env1 -> evalAST i (FuncRes asts) (env1 ++ env)
            Nothing -> Left $ "function " ++ func ++ " wrong arguments"
        _ -> Left $ "function " ++ func ++ " not found"

evalFunction :: Int -> [Ast] -> [Env] -> Either String ([Ast], [Env])
evalFunction _ [] env = Right ([], env)
evalFunction i (x:xs) env = case evalAST i x env of
    Left err -> Left err
    Right ((Print ast), env1) -> case evalFunction i xs env1 of
        Left err -> Left err
        Right (asts, env2) -> Right ((Print ast) : asts, env2)
    Right ((FuncRes asts), env1) -> case evalFunction i asts env1 of
        Left err -> Left err
        Right (asts1, env2) -> case evalFunction i xs env2 of
            Left err -> Left err
            Right (asts2, env3) -> Right (asts1 ++ asts2, env3)
    Right ((IfRes asts), env1) -> case evalFunction i asts env1 of
        Left err -> Left err
        Right (asts1, env2) -> case evalFunction i xs env2 of
            Left err -> Left err
            Right (asts2, env3) -> Right (asts1 ++ asts2, env3)
    Right (_, env1) -> case evalFunction i xs env1 of
        Left err -> Left err
        Right (asts, env2) -> Right (asts, env2)

evalPrint :: Int -> [Ast] -> [Env] -> Either String [Ast]
evalPrint _ [] _ = Right []
evalPrint i (x:xs) env =
    case evalAST i x env of
        Left err -> Left err
        Right (ast, _) ->
            case evalPrint i xs env of
                            Left err -> Left err
                            Right asts -> Right $ ast : asts

evalAST :: Int -> Ast -> [Env] -> Either String (Ast, [Env])
evalAST 1000 _ _ = Left "stack overflow"
evalAST j ast env = let i = j + 1 in
            case ast of
                (Define name ast1) -> Right (ast1, (Var name ast1) : env)
                (Func name args asts) ->
                    Right (ast, (FuncVar name args asts) : env)
                (FuncRes asts) -> case evalFunction i asts env of
                    Left err -> Left err
                    Right (asts1, _) -> Right (FuncRes asts1, env)
                (IfRes asts) -> case evalFunction i asts env of
                    Left err -> Left err
                    Right (asts1, env1) -> Right (IfRes asts1, env1)
                (Call "add" _) -> plus $ evalASTCall i ast env
                (Call "mul" _) -> mul $ evalASTCall i ast env
                (Call "div" _) -> myDiv $ evalASTCall i ast env
                (Call "mod" _) -> myMod $ evalASTCall i ast env
                (Call "sub" _) -> minus $ evalASTCall i ast env
                (Call "equal" _) -> equal $ evalASTCall i ast env
                (Call "ne" _) -> notequal $ evalASTCall i ast env
                (Call "leq" _) -> myLeq $ evalASTCall i ast env
                (Call "geq" _) -> myGeq $ evalASTCall i ast env
                (Call "lower" _) -> lower $ evalASTCall i ast env
                (Call "greater" _) -> greater $ evalASTCall i ast env
                (Call "fact" _) -> fact $ evalASTCall i ast env
                (If _ _ _) -> evalASTIfCond i ast env
                (While _ _) -> case evalASTWhileCond i ast env of
                                    Left err -> Left err
                                    Right (asts1, env1) ->
                                         Right (FuncRes asts1, env1)
                (Call "print" asts) -> case evalPrint i asts env of
                    Left err -> Left err
                    Right asts1 -> Right (Print asts1, env)
                (Call func args) -> getFuncASTInEnv i func args env
                (Symbol str) -> case getASTInEnv str env of
                    Just (ast1, env1) -> evalAST i ast1 env1
                    Nothing -> Left $ "variable " ++ str ++ " not found"
                _ -> Right (ast, env)
