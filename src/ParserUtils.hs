{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- ParserUtils.hs
-}

module ParserUtils (parseChar, parseNumber, parseVar) where

import Types

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
    case str of
        (x:xs) | x == c -> Just (c, xs)
        _ -> Nothing

isInList :: Char -> String -> Bool
isInList _ [] = False
isInList c (x:xs) | x == c = True
                  | otherwise = isInList c xs

parseAnyChar :: String -> Parser Char
parseAnyChar list = Parser $ \str ->
    case str of
        (x:xs) | isInList x list -> Just (x, xs)
        _ -> Nothing

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser $ \str ->
    case runParser parser str of
        Just (res, rest) ->
            case runParser (parseSome parser) rest of
                Just (res1, rest1) -> Just (res : res1, rest1)
                Nothing -> Just ([res], rest)
        Nothing -> Nothing

parseNumber :: Parser String
parseNumber = (parseSome (parseAnyChar ['0'..'9']))

parseVar :: Parser String
parseVar = (parseSome (parseAnyChar $ '_' : ['A'..'Z'] ++ ['a'..'z']))
