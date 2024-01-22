{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Types

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs)
    | x `elem` " \t\r\n" = ([], xs)
    | otherwise = let (str, rest) = firstWord xs in
                  (x : str, rest)

parseAsParent :: String -> Maybe (String, String)
parseAsParent [] = Just ([], [])
parseAsParent (x:xs)
    | x == '(' = Just ([], xs)
    | otherwise = case parseAsParent xs of
        Just (str, rest) -> Just (x : str, rest)
        Nothing -> Nothing

checkLetter :: Char -> Bool
checkLetter x = (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z' )

parseWord :: String -> Maybe (String, String)
parseWord [] = Just ([], [])
parseWord (x:xs)
    | x == ',' || x == ')' = Just ([], xs)
    | checkLetter x =  case parseWord xs of
        Just (str, rest) -> Just (x : str, rest)
        Nothing -> Nothing
    | otherwise = Nothing

parseParams :: String -> Maybe ([String], String)
parseParams [] = Just([], [])
parseParams str = case parseWord str of
    Just (arg, rest) -> case parseParams rest of
        Just (args, rest1) -> Just (arg : args, rest1)
        Nothing -> Nothing
    Nothing -> Nothing

parseFunc :: String -> Maybe Ast
parseFunc [] = Nothing
parseFunc str = case parseAsParent str of
    Just (name, rest) -> case parseParams rest of
        Just (args, rest1) -> Just $ Call2 name args []
        Nothing -> Nothing
    Nothing -> Nothing

parse :: String -> [Ast]
parse [] = []
parse (';':xs) = parse xs
parse (' ':xs) = parse xs
parse ('\t':xs) = parse xs
parse ('\r':xs) = parse xs
parse ('\n':xs) = parse xs
parse str = case firstWord str of
                ("func", rest) -> case parseFunc rest of
                    Just ast -> [ast]
                    Nothing -> []
                _ -> []