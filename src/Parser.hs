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

parseOr :: Parser Ast
parseOr = parseInt <* parseChar ';'
      <|> parseSymbol <* parseChar ';'
      <|> parseString <* parseChar ';'

parse :: String -> Either String [Ast]
parse [] = Right []
parse (';':xs) = parse xs
parse (' ':xs) = parse xs
parse ('\t':xs) = parse xs
parse ('\r':xs) = parse xs
parse ('\n':xs) = parse xs
parse str =
    case runParser parseOr str of
        Just (ast, rest) ->
            case parse rest of
                Left err -> Left err
                Right asts -> Right $ ast : asts
        Nothing -> Left "err"
