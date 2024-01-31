{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- ParserUtils.hs
-}

module ParserUtils (parseChar, parseNumber, parseVar, parseStr, parseSep,
parseSpaces, parseList, parseListIf, parseWord, isInList, parseAnyChar,
parseMany, parseSome, parseNumberVar, parseListArgs, parseListArgsIf) where

import Control.Applicative
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

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser $ \str ->
    case runParser parser str of
        Just (res, rest) ->
            case runParser (parseMany parser) rest of
                Just (res1, rest1) -> Just (res : res1, rest1)
                Nothing -> Just ([res], rest)
        Nothing -> Just ([], str)

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
          <|> (:) <$> (parseChar '-') <*> (parseSome (parseAnyChar ['0'..'9']))

parseVar :: Parser String
parseVar = (:) <$> (parseAnyChar list)
               <*> (parseMany (parseAnyChar $ list ++ ['0'..'9']))
    where
        list = '_' : ['A'..'Z'] ++ ['a'..'z']

parseNumberVar :: Parser String
parseNumberVar = (:) <$> (parseAnyChar list)
                 <*> (parseMany (parseAnyChar list))
  where
    list = ['_', 'A'..'Z'] ++  ['a'..'z'] ++ ['0'..'9']

parseStr :: Parser String
parseStr = Parser $ \str ->
    case str of
        [] -> Nothing
        ('"':rest) -> Just ([], '"' : rest)
        ('\\':'"':xs) -> f '"' xs
        (x:xs) -> f x xs
    where
        f x xs =
            case runParser parseStr xs of
                Just (res, rest) -> Just (x : res, rest)
                Nothing -> Nothing

parseSep :: Parser String
parseSep = parseSpaces <* parseChar ';'

parseSpaces :: Parser String
parseSpaces = parseMany (parseChar ' ')

parseListArgs :: Parser [String]
parseListArgs = Parser $ \str ->
    case str of
        [] -> Nothing
        (' ':xs) -> runParser parseListArgs xs
        ('\t':xs) -> runParser parseListArgs xs
        ('\r':xs) -> runParser parseListArgs xs
        ('\n':xs) -> runParser parseListArgs xs
        (')':xs) -> Just ([], ')' : xs)
        _ -> runParser parseVar str >>= \(var, str1) ->
            case runParser parseSpaces str1 of
                Just (_, (',':xs)) -> fct var xs
                Just (_, (')':xs)) -> fct var (')' : xs)
                _ -> Nothing
    where
        fct var rest =
            case runParser parseListArgs rest of
                Just (var1, rest1) -> Just (var : var1, rest1)
                Nothing -> Nothing

parseListArgsIf :: Parser [String]
parseListArgsIf = Parser $ \str ->
    case str of
        [] -> Nothing
        (' ':xs) -> runParser parseListArgsIf xs
        ('\t':xs) -> runParser parseListArgsIf xs
        ('\r':xs) -> runParser parseListArgsIf xs
        ('\n':xs) -> runParser parseListArgsIf xs
        (')':xs) -> Just ([], ')' : xs)
        _ -> runParser parseNumberVar str >>= \(var, str1) ->
            case runParser parseSpaces str1 of
                Just (_, (',':xs)) -> fct var xs
                Just (_, (')':xs)) -> fct var (')' : xs)
                _ -> Nothing
    where
        fct var rest =
            case runParser parseListArgsIf rest of
                Just (var1, rest1) -> Just (var : var1, rest1)
                Nothing -> Nothing

parseList :: Parser [String]
parseList = Parser $ \str ->
    case str of
        [] -> Nothing
        (')':xs) -> Just ([], ')' : xs)
        _ ->
            case runParser parseListArgs str of
                Just (args, rest) -> Just (args, rest)
                Nothing -> Nothing

parseListIf :: Parser [String]
parseListIf = Parser $ \str ->
    case str of
        [] -> Nothing
        (')':xs) -> Just ([], ')' : xs)
        _ ->
            case runParser parseListArgsIf str of
                Just (args, rest) -> Just (args, rest)
                Nothing -> Nothing

parseWord :: String -> Parser String
parseWord [] = Parser $ \str -> Just ([], str)
parseWord (w:ws) = Parser $ \str ->
    case str of
        (x:xs) | x == w ->
            case runParser (parseWord ws) xs of
                Just (word, rest) -> Just ((w : word), rest)
                Nothing -> Nothing
        _ -> Nothing
