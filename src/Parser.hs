{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Control.Applicative
import Types
import Utils

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str ->
        case runParser parser str of
            Just (res, rest) -> Just (fct res, rest)
            Nothing -> Nothing

instance Applicative Parser where
    pure res = Parser $ \rest -> Just (res, rest)
    parserf <*> parser = Parser $ \str ->
        case runParser parserf str of
            Just (fct, rest) ->
                case runParser parser rest of
                    Just (res, rest1) -> Just (fct res, rest1)
                    Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    parser1 <|> parser2 = Parser $ \str ->
        case runParser parser1 str of
            Just (res, rest) -> Just (res, rest)
            Nothing -> runParser parser2 str

parse :: String -> Either String [Ast]
parse [] = Right []
parse _ = Left "test"
