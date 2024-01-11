{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Glados.hs
-}

module Glados (SExpr(..), getSymbol, getInteger, getList, printTree, printTreeList, sexprToAST, parseChar, parseAnyChar, parseOr, parseAnd, interpreter, start) where

import Eval (evalAST)
import Parser (parse)
import CommandLines
import Print
import System.IO
import Types

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
                    Left err -> putStrLn err
                    Right ast -> putStrLn (prettyPrint ast) >> printAST xs env



interpreter :: [Env] -> IO ()
interpreter env = do
    eof <- isEOF
    if eof
        then return ()
        else do
            line <- getLine
            case line of
                "!quit" -> quitCommand
                "!man" -> do
                    manCommand
                    interpreter env
                "!help" -> do
                    helpCommand
                    interpreter env
                _ -> do
                    printAST (parse line) env
                    interpreter env

start :: IO ()
start = do
    interpreter []
