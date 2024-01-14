{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Glados.hs
-}

module Glados (SExpr(..), getSymbol, getInteger, getList, printTree, printTreeList, sexprToAST, parseChar, parseAnyChar, parseOr, parseAnd, interpreter, start) where

import CommandLines
import Compiler
import Eval (evalAST)
import Parser (parse)
import Print
import System.Environment
import System.IO
import Types
import VM

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving Show

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol expr) = Just expr
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt expr) = Just expr
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList expr) = Just expr
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SInt expr) = Just $ "a Number " ++ show expr
printTree (SSymbol expr) = Just $ "a Symbol '" ++ expr ++ "'"
printTree (SList []) = Just "an empty List"
printTree (SList (x:xs)) = case printTree x of
                                Just expr -> Just $ "(a List with " ++ expr ++ " followed by " ++ printTreeList xs ++ ")"
                                Nothing -> Just $ "a List with nothing followed by " ++ printTreeList xs

printTreeList :: [SExpr] -> String
printTreeList [] = "nothing"
printTreeList [x] = case printTree x of
                        Just str -> str
                        Nothing -> "nothing"
printTreeList (x:xs) = case printTree x of
                           Just str -> str ++ ", " ++ printTreeList xs
                           Nothing -> printTreeList xs

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt i) = Just $ IntLiteral i
sexprToAST (SSymbol i) = Just $ StringLiteral i
sexprToAST (SList [SSymbol "define", SSymbol symbol, SInt i]) = Just $ Define symbol (IntLiteral i)
sexprToAST (SList [SSymbol "define", SSymbol symbol, SSymbol s]) = Just $ Define symbol (StringLiteral s)
sexprToAST _ = Nothing

type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar c (x:xs) | x == c = Just (c, xs)
                   | otherwise = Nothing
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar list (x:xs) | x `elem` list = Just (x, xs)
                         | otherwise = Nothing
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 list = case p1 list of
                        Just p -> Just p
                        Nothing -> p2 list

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 list = case p1 list of
                        Just (c1, list1) -> case p2 list1 of
                                                Just (c2, list2) -> Just ((c1, c2), list2)
                                                Nothing -> Nothing
                        Nothing -> Nothing

printAST :: [Ast] -> [Env] -> IO ()
printAST [] _ = return ()
printAST (x:xs) env = case evalAST x env of
                    Left err -> putStrLn ("Exception: " ++ err ++ " " ++ (prettyPrint x))
                    Right ast -> putStrLn (prettyPrint ast) >> printAST xs env

foundDefine :: [Ast] -> Maybe Env
foundDefine [(Define str ast)] = Just (Var str ast)
foundDefine _ = Nothing

parseEnv :: String -> [Env] -> Maybe String -> IO ()
parseEnv line env file = do
    case foundDefine asts of
        Just var -> interpreter (var : env) file
        Nothing -> case file of
                    Just path -> (compile asts path)
                    Nothing -> (printAST asts env) >> interpreter env file
    where
        asts = (parse line)

interpreter :: [Env] -> Maybe String -> IO ()
interpreter env file = do
    eof <- isEOF
    if eof
        then return ()
        else do
            line <- getLine
            case line of
                "!quit" -> quitCommand
                "!man" -> do
                    manCommand
                    interpreter env file
                "!help" -> do
                    helpCommand
                    interpreter env file
                _ -> do
                    parseEnv line env file

start :: IO ()
start = do
    args <- getArgs
    case args of
        ("--compiler":file:_) -> startInterpreter $ Just file
        ("--compiler":_) -> putStrLn "--compiler argument need a file"
        ("--vm":file:_) -> case exec [(CallOp DIV)] [(IntVM 42), (IntVM 0)] of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right value -> putStrLn $ show $ value
        ("--vm":_) -> putStrLn "--vm argument need a file"
        _ -> startInterpreter Nothing
    where startInterpreter = interpreter [(Var "eq?" (Lambda ["x", "y"] (Call "?" [(Symbol "x"), (Symbol "y")])))
                                         ,(Var "div" (Lambda ["x", "y"] (Call "/" [(Symbol "x"), (Symbol "y")])))
                                         ,(Var "mod" (Lambda ["x", "y"] (Call "%" [(Symbol "x"), (Symbol "y")])))]
