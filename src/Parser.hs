{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (isNumber, isStringNumber, firstWord, parseList, parseCall, parseString, parseIntSym, parse) where

import Types
import Glados

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

isStringNumber :: String -> Bool
isStringNumber [] = False
isStringNumber [x] = isNumber x
isStringNumber (x:xs) = isNumber x && isStringNumber xs

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs) | x == ' ' || x == '\t' || x == '\r' || x == '\n' = ([], xs)
                 | otherwise = let (str, rest) = (firstWord xs) in
                               (x : str, rest)

parseList :: (Int, String) -> (String, String)
parseList (_, []) = ([], [])
parseList (0, rest) = ([], rest)
parseList (i, (x:xs)) | x == '(' = let (list, rest) = parseList (i + 1, xs) in
                                   (x : list, rest)
                      | x == ')' = let (list, rest) = parseList (i - 1, xs) in
                                   if i == 1
                                   then (list, rest)
                                   else (x : list, rest)
                      | otherwise = let (list, rest) = parseList (i, xs) in
                                    (x : list, rest)

parseDefine :: String -> Maybe Ast
parseDefine args = case parse args of
                    [(StringLiteral var), ast] -> Just $ Define var ast
                    [(Symbol var), ast] -> Just $ Define var ast
                    _ -> Nothing

parseCall :: String -> (Maybe Ast, String)
parseCall str = let (list, rest) = parseList (1, str) in
                case firstWord list of
                    ([], _) -> (Nothing, rest)
                    ("define", args) -> (parseDefine args, rest)
                    (word, args) -> (Just $ Call word (parse args), rest)

parseString :: String -> (String, String)
parseString [] = ([], [])
parseString ('"':xs) = ([], xs)
parseString ('\\':'"':xs) = let (str, rest) = (parseString xs) in
                            ('"' : str, rest)
parseString (x:xs) = let (str, rest) = (parseString xs) in
                     (x : str, rest)

parseIntSym :: String -> Ast
parseIntSym ('-':xs) | isStringNumber xs = IntLiteral (((read xs) :: Int) * (-1))
parseIntSym str      | isStringNumber str = IntLiteral ((read str) :: Int)
parseIntSym str      = Symbol str

parseLambda :: String -> Maybe Ast
parseLambda args = case parse args of
                    [(SSymbol "lambda"), (SList params), body] ->
                        Just $ Lambda (map (\(SSymbol param) -> param) params) (sexprToAST body)
                    _ -> Nothing

parse :: String -> [Ast]
parse [] = []
parse (' ':xs) = parse xs
parse ('\t':xs) = parse xs
parse ('\r':xs) = parse xs
parse ('\n':xs) = parse xs
parse ('(':xs) = case parseCall xs of
                    (Just ast, rest) -> ast : (parse rest)
                    (Nothing, rest) -> (parse rest)
parse ('"':xs) = case parseString xs of
                    ([], _) -> []
                    (str, rest) -> (StringLiteral str) : (parse rest)
parse ('#':'t':xs) = (BoolLiteral True) : (parse xs)
parse ('#':'f':xs) = (BoolLiteral False) : (parse xs)
parse str = case firstWord str of
                ([], _) -> []
                ("lambda", rest) -> case parseLambda rest of
                                      Just ast -> ast : parse (drop (length rest) str)
                                      Nothing -> []
                (word, rest) -> (parseIntSym word) : (parse rest)