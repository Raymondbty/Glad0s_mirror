{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Glados.hs
-}

module Glados (start) where

import Ast
import Funcs
import Parser (parse)
import Print
import System.IO

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

evalAST :: Ast -> Either String Ast
evalAST ast = case ast of
                (Call "+" _) -> plus ast
                (Call "-" _) -> minus ast
                (Call "*" _) -> mul ast
                (Call "/" _) -> myDiv ast
                (Call "div" _) -> myDiv ast
                (Call "%" _) -> myMod ast
                (Call "mod" _) -> myMod ast
                (Call _ _) -> Left $ noMatchingFunction ast
                _ -> Right ast

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

printAST :: [Ast] -> IO ()
printAST [] = return ()
printAST (x:xs) = case evalAST x of
                    Left err -> putStrLn err
                    Right ast -> putStrLn (prettyPrint ast) >> printAST xs

interpreter :: IO ()
interpreter = do
    eof <- isEOF
    if eof
        then return ()
        else do
            line <- getLine
            printAST $ parse line
            interpreter

start :: IO ()
start = do
    interpreter
