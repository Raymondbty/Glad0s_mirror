{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Control.Applicative
import ParserUtils
import Types

parseInt :: Parser Ast
parseInt = Parser $ \str ->
    case runParser parseNumber str of
        Just (res, rest) -> Just (IntLiteral $ ((read res) :: Int), rest)
        Nothing -> Nothing

parseSymbol :: Parser Ast
parseSymbol = Parser $ \str ->
    case runParser parseVar str of
        Just (res, rest) -> Just (Symbol res, rest)
        Nothing -> Nothing

parseStringContent :: Parser Ast
parseStringContent = Parser $ \str ->
    case runParser parseStr str of
        Just (res, rest) -> Just (StringLiteral res, rest)
        Nothing -> Nothing

parseString :: Parser Ast
parseString = parseChar '"' *> parseStringContent <* parseChar '"'

parseFuncContent :: Parser Ast
parseFuncContent = Parser $ \str ->
    case runParser parse str of
        Just (asts, rest) -> Just (Func "tst" asts, rest)
        Nothing -> Nothing

parseFunc :: Parser Ast
parseFunc = parseChar '{' *> parseFuncContent <* parseChar '}'

parseOr :: Parser Ast
parseOr = parseInt <* parseSep
      <|> parseSymbol <* parseSep
      <|> parseString <* parseSep
      <|> parseFunc

parse :: Parser [Ast]
parse = Parser $ \str ->
    case str of
        [] -> Just ([], [])
        ('}':xs) -> Just ([], '}' : xs)
        (';':xs) -> runParser parse xs
        (' ':xs) -> runParser parse xs
        ('\t':xs) -> runParser parse xs
        ('\r':xs) -> runParser parse xs
        ('\n':xs) -> runParser parse xs
        _ ->
            case runParser parseOr str of
                Just (ast, rest) ->
                    case runParser parse rest of
                        Just (asts, rest1) -> Just (ast : asts, rest1)
                        Nothing -> Nothing
                Nothing -> Nothing
