{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Ast
import Data.Char

isStringNumber :: String -> Bool
isStringNumber [] = False
isStringNumber [x] = isNumber x
isStringNumber (x:xs) = isNumber x && isStringNumber xs

removeLastElem :: String -> String
removeLastElem [] = []
removeLastElem [_] = []
removeLastElem (x:xs) = x : removeLastElem xs

parseArgs :: [String] -> [Ast]
parseArgs [] = []
parseArgs (x:xs) = (parse x) : (parseArgs xs)

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs) | x == ' ' = ([], xs)
                 | otherwise = let (str, rest) = (firstWord xs) in
                               (x : str, rest)

parseCall :: String -> Ast
parseCall str = case firstWord str of
                    ([], _) -> StringLiteral ""
                    (word, rest) -> Call word (parseArgs rest)

parseAst :: String -> [Ast]
parseAst [] = []
parseAst (x:xs) | x == '(' && null xs == False && (head $ reverse xs) == ')' = parseCall $ removeLastElem xs
                | isStringNumber (x : xs) = IntLiteral (read (x : xs) :: Int)
                | otherwise = StringLiteral (x : xs)

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
parse ('"':xs) = case parseString xs of
                    ([], _) -> []
                    (str, rest) -> (StringLiteral str) : (parse rest)
parse str = case firstWord str of
                ([], _) -> []
                (word, rest) -> (parseIntSym word) : (parse rest)
