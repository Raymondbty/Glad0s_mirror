{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import ParserUtils
import Types

parseInt :: Parser Ast
parseInt = Parser $ \str ->
    case runParser (parseSome (parseAnyChar ['0'..'9'])) str of
        Just (res, rest) -> Just (IntLiteral $ ((read res) :: Int), rest)
        Nothing -> Nothing

parseOr :: Parser Ast
parseOr = parseInt

parse :: String -> Either String [Ast]
parse [] = Right []
parse str =
    case runParser parseOr str of
        Just (ast, rest) ->
            case parse rest of
                Left err -> Left err
                Right asts -> Right $ ast : asts
        Nothing -> Left "err"
