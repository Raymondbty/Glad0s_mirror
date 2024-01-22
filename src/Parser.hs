{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Types

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
