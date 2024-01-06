{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Ast

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

isStringNumber :: String -> Bool
isStringNumber [] = False
isStringNumber [x] = isNumber x
isStringNumber (x:xs) = isNumber x && isStringNumber xs

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs) | x == ' ' = ([], xs)
                 | otherwise = let (str, rest) = (firstWord xs) in
                               (x : str, rest)

parseList :: String -> (String, String)
parseList str = ("", "")

parseCall :: String -> (Maybe Ast, String)
parseCall str = let (list, rest) = parseList str in
                case firstWord list of
                    ([], _) -> (Nothing, rest)
                    (word, args) -> (Just $ Call word (parse args), rest)

parseString :: String -> (String, String)
parseString [] = ([], [])
parseString (x:xs) | x == '"' = ([], xs)
                   | otherwise = let (str, rest) = (parseString xs) in
                                 (x : str, rest)

parseIntSym :: String -> Ast
parseIntSym ('-':xs) | isStringNumber xs = IntLiteral (((read xs) :: Int) * (-1))
parseIntSym str      | isStringNumber str = IntLiteral ((read str) :: Int)
parseIntSym str      = Symbol str

parse :: String -> [Ast]
parse [] = []
parse (' ':xs) = parse xs
parse ('(':xs) = case parseCall xs of
                    (Just ast, rest) -> ast : (parse rest)
                    (Nothing, rest) -> (parse rest)
parse ('"':xs) = case parseString xs of
                    ([], _) -> []
                    (str, rest) -> (StringLiteral str) : (parse rest)
parse str = case firstWord str of
                ([], _) -> []
                (word, rest) -> (parseIntSym word) : (parse rest)
