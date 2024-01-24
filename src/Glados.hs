{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Glados.hs
-}

module Glados (SExpr(..), getSymbol, getInteger, getList, printTree, printTreeList, sexprToAST, parseChar, parseAnyChar, parseOr, parseAnd, start) where

import CommandLines
import Compiler
import Disassembler
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
    Just expr -> Just $ "(a List with " ++ expr ++ " followed by " ++
                        printTreeList xs ++ ")"
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
sexprToAST (SList [SSymbol "define", SSymbol symbol, SInt i]) =
    Just $ Define symbol (IntLiteral i)
sexprToAST (SList [SSymbol "define", SSymbol symbol, SSymbol s]) =
    Just $ Define symbol (StringLiteral s)
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
                    Left err -> putStrLn ("Exception: " ++ err ++ " " ++
                        (prettyPrint x))
                    Right ast -> putStrLn (prettyPrint ast) >> printAST xs env

foundDefine :: [Ast] -> Maybe Env
foundDefine [(Define str ast)] = Just (Var str ast)
foundDefine _ = Nothing

parseEnv :: String -> [Env] -> Maybe String -> IO ()
parseEnv line env file =
    case file of
        Just path -> compile asts path
        Nothing -> printAST asts env
  where
    asts = parse line

getInput :: IO (String)
getInput = isEOF >>= \eof ->
    case eof of
        True -> return []
        False -> getLine >>= \line ->
            getInput >>= \rest ->
            return $ line ++ rest

start :: IO ()
start = getArgs >>= \args ->
    case args of
        ("--compile":file:_) -> startCompile file
        ("--compile":_)      -> putStrLn "--compile argument needs a file"
        ("--disassemble":file:_) -> disassemble file
        ("--disassemble":_)      -> putStrLn "--disassemble needs a file"
        ("--vm":file:_)      -> startVM file
        ("--vm":_)           -> putStrLn "--vm argument needs a file"
        ("--man":_)          -> manCommand
        ("--help":_)         -> helpCommand
        _                    -> startInterpreter Nothing

startCompile :: String -> IO ()
startCompile file = startInterpreter $ Just file

startInterpreter :: Maybe String -> IO ()
startInterpreter file = getInput >>= \input ->
    parseEnv input initialEnv file

initialEnv :: [Env]
initialEnv =
    [ Var "eq?" (Lambda ["x", "y"] (Call "?" [(Symbol "x"), (Symbol "y")])),
      Var "div" (Lambda ["x", "y"] (Call "/" [(Symbol "x"), (Symbol "y")])),
      Var "mod" (Lambda ["x", "y"] (Call "%" [(Symbol "x"), (Symbol "y")])),
      Var "fact" (Lambda ["n"] (Call "!" [(Symbol "n")]))
    ]
