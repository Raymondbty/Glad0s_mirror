{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast.hs
-}

module Eval (evalASTS) where

import Funcs
import Print
import System.Exit
import Types

evalCallPrint :: Int -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalCallPrint _ [] env = (putStrLn []) >> (return $ Right (Void, env))
evalCallPrint stack asts env =
    evalCallArgs stack asts env >>= \evalArgs ->
        case evalArgs of
            Left err -> return $ Left err
            Right asts1 -> (putStrLn $ prettyPrint asts1)
                        >> (return $ Right (Void, env))

evalCallArgs :: Int -> [Ast] -> [Env] -> IO (Either String [Ast])
evalCallArgs _ [] _ = return $ Right []
evalCallArgs stack (x:xs) env =
    evalAST stack x env >>= \evalX ->
        case evalX of
            Left err -> return $ Left err
            Right (ast, env1) ->
                evalCallArgs stack xs env1 >>= \evalArgs ->
                case evalArgs of
                    Left err -> return $ Left err
                    Right asts -> return $ Right $ ast : asts

evalCallFunc :: Int -> ([Ast] -> Either String Ast) -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalCallFunc stack func asts env =
    evalCallArgs stack asts env >>= \evalArgs ->
        case evalArgs of
            Left err -> return $ Left err
            Right asts1 ->
                case func asts1 of
                    Left err -> return $ Left err
                    Right ast -> return $ Right (ast, env)

evalCallFindUser :: String -> [Env] -> Maybe ([String], [Ast])
evalCallFindUser _ [] = Nothing
evalCallFindUser name ((FuncVar var params content):_) | var == name =
    Just (params, content)
evalCallFindUser name (_:xs) = evalCallFindUser name xs

evalCall :: Int -> String -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalCall stack name asts env =
    case evalCallFindUser name env of
        Just func -> evalCallUser stack func asts env >>= \evalUserDefined ->
            case evalUserDefined of
                Left err -> return $ Left $ "function " ++ name ++ ": " ++ err
                Right (ast1, env1) -> return $ Right (ast1, env1)
        Nothing -> evalCallBuiltin stack name asts env

evalCallUserEnv :: [Ast] -> [String] -> Either String [Env]
evalCallUserEnv [] [] = Right []
evalCallUserEnv [] _ = Left "not enough params"
evalCallUserEnv _ [] = Left "too much params"
evalCallUserEnv (a:as) (p:ps) =
    case evalCallUserEnv as ps of
        Left err -> Left err
        Right env -> Right $ (Var p a) : env

evalCallUser :: Int -> ([String], [Ast]) -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalCallUser stack (params, content) asts env =
    case evalCallUserEnv asts params of
        Left err -> return $ Left err
        Right env1 ->
            evalASTS (stack + 1) content (env1 ++ env) >>= \ret ->
                return $ Right (ret, env)

evalCallBuiltin :: Int -> String -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalCallBuiltin stack "add" asts env = evalCallFunc stack add asts env
evalCallBuiltin stack "sub" asts env = evalCallFunc stack sub asts env
evalCallBuiltin stack "mul" asts env = evalCallFunc stack mul asts env
evalCallBuiltin stack "div" asts env = evalCallFunc stack myDiv asts env
evalCallBuiltin stack "mod" asts env = evalCallFunc stack myMod asts env
evalCallBuiltin stack "leq" asts env = evalCallFunc stack myLeq asts env
evalCallBuiltin stack "geq" asts env = evalCallFunc stack myGeq asts env
evalCallBuiltin stack "equal" asts env = evalCallFunc stack equal asts env
evalCallBuiltin stack "ne" asts env = evalCallFunc stack notequal asts env
evalCallBuiltin stack "lower" asts env = evalCallFunc stack lower asts env
evalCallBuiltin stack "greater" asts env = evalCallFunc stack greater asts env
evalCallBuiltin _ name _ _ = return $ Left $ "function not found: " ++ name

evalSymbol :: Int -> String -> [Env] -> IO (Either String (Ast, [Env]))
evalSymbol _ sym [] = return $ Left $ "symbol not found: " ++ sym
evalSymbol stack sym ((Var var ast):xs) | var == sym =
    evalAST stack ast xs >>= \eval ->
        case eval of
            Left err -> return $ Left err
            Right (ast1, env) -> return $ Right (ast1, env)
evalSymbol stack sym (_:xs) = evalSymbol stack sym xs

evalIf :: Int -> Ast -> [Ast] -> [Ast] -> [Env] -> IO (Either String (Ast, [Env]))
evalIf stack cond trueBranch falseBranch env =
    evalAST stack cond env >>= \evalCond ->
        case evalCond of
            Left err -> return $ Left err
            Right (BoolLiteral True, _) -> evalBranch trueBranch
            Right (BoolLiteral False, _) -> evalBranch falseBranch
            Right (IntLiteral 0, _) -> evalBranch falseBranch
            Right (IntLiteral _, _) -> evalBranch trueBranch
            _ -> return $ Left "if condition must be an integer or a boolean"
    where
        evalBranch branch =
            evalASTS stack branch env >>= \ret ->
                    return $ Right (ret, env)

evalAST :: Int -> Ast -> [Env] -> IO (Either String (Ast, [Env]))
evalAST _ (Define name ast1) env = return $ Right (Void, (Var name ast1) : env)
evalAST _ (Func name args asts) env =
    return $ Right (Void, (FuncVar name args asts) : env)
evalAST stack (Call "print" asts) env = evalCallPrint stack asts env
evalAST stack (Call name asts) env = evalCall stack name asts env
evalAST stack (Symbol sym) env = evalSymbol stack sym env
evalAST stack (If cond trueBranch falseBranch) env =
    evalIf stack cond trueBranch falseBranch env
evalAST _ ast env = return $ Right (ast, env)

evalASTS :: Int -> [Ast] -> [Env] -> IO (Ast)
evalASTS 1000 _ _ = putStrLn "Exception: Stack overflow"
                 >> exitWith (ExitFailure 84)
evalASTS _ [] _ = return (Void)
evalASTS stack (x:xs) env =
    evalAST stack x env >>= \evaluation ->
        case evaluation of
            Left err -> (putStrLn $ "Exception: " ++ err)
                >> exitWith (ExitFailure 84)
            Right (Return ast, _) -> return ast
            Right (_, env1) -> evalASTS stack xs env1
