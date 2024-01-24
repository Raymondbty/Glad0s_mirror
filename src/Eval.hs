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

evalASTS :: [Ast] -> [Env] -> [Ast]
evalASTS [] env = []
evalASTS(x:xs) env = case evalAST x env of
                    Left _ -> []
                    Right (ast, _) -> ast : (evalASTS xs env)

getASTInEnv :: String -> [Env] -> Maybe Ast
getASTInEnv _ [] = Nothing
getASTInEnv str ((Var key ast):xs) = if str == key
                                     then Just ast
                                     else getASTInEnv str xs
getASTInEnv str ((FuncVar key asts):xs) = if str == key
                                     then Just $ FuncRes $ evalASTS asts xs
                                     else getASTInEnv str xs

lookSymbolInEnv :: String -> [Env] -> Either String (Ast, [Env])
lookSymbolInEnv str env = case getASTInEnv str env of
    Just ast -> evalAST ast env
    Nothing -> Left $ "variable " ++ str ++ " not found"

appendVars :: [String] -> [Ast] -> Either String [Env]
appendVars [] [] = Right []
appendVars [] _ = Left "incorrect number of arguments"
appendVars _ [] = Left "incorrect number of arguments"
appendVars (_:vx) ((Symbol _):ax) = appendVars vx ax
appendVars (v:vx) (a:ax) = case appendVars vx ax of
                            Left err -> Left err
                            Right res -> Right $ (Var v a) : res

checkFunc :: String -> [Ast] -> [Env] -> Either String (Ast, [Env])
checkFunc func args env = case lookSymbolInEnv func env of
                            Left err -> Left err
                            Right ((Lambda vars ast), _) ->
                                case appendVars vars args of
                                    Left err -> Left err
                                    Right parms -> evalAST ast (parms ++ env)
                            _ -> Left "no matching function"

evalAST :: Ast -> [Env] -> Either String (Ast, [Env])
evalAST ast env = case ast of
                (Print ast) -> case evalAST ast env of
                    Left err -> Left err
                    Right (ast, env1) -> Right (Print ast, env1)
                (Define name ast) -> Right (ast, (Var name ast) : env)
                (Func name args) -> Right (ast, (FuncVar name args) : env)
                (Call "add" _) -> plus $ evalASTCall ast env
                (Call func args) -> lookSymbolInEnv func env
                (Symbol str) -> lookSymbolInEnv str env
                _ -> Right (ast, env)
