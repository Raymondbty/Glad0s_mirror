{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse, parseBool, parseInt, parseSymbol, parseStringContent,
parseString, parseFuncContent) where

import Debug.Trace
import Control.Applicative
import ParserUtils
import Types

parseBool :: Parser Ast
parseBool = Parser $ \str ->
    case runParser parseVar str of
        Just ("true", rest) -> Just (BoolLiteral True, rest)
        Just ("false", rest) -> Just (BoolLiteral False, rest)
        _ -> Nothing

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

parseFuncContent :: String -> [String] -> Parser Ast
parseFuncContent name args = Parser $ \str ->
    case runParser parse str of
        Just (asts, rest) -> Just (Func name args asts, rest)
        Nothing -> Nothing

parseFunc :: Parser Ast
parseFunc =
    parseWord "func " *> parseSpaces *>
    parseVar >>= \name ->
        parseSpaces *> parseChar '(' *> parseSpaces *> parseList >>= \var ->
            parseChar ')' *> parseSpaces *> parseChar '{' *> parseFuncContent
            name var <* parseChar '}'

parseIfContent :: [String] -> Parser Ast
parseIfContent args = Parser $ \str -> trace (show str) $
    case runParser parse str of
        Just (asts, rest) -> Just (If args asts, rest)
        Nothing -> Nothing

parseIf :: Parser Ast
parseIf =
    parseWord "if " *>
    parseSpaces *> parseChar '(' *> parseSpaces *> parseListIf >>= \var -> trace (show var) $
    parseChar ')' *> parseSpaces *> parseChar '{' *> parseIfContent
    var <* parseChar '}'

parseFuncCallContent :: String -> Parser Ast
parseFuncCallContent name = Parser $ \str ->
    case runParser parseListCall str of
        Just (asts, rest) -> Just (Call name asts, rest)
        Nothing -> Nothing

parseFuncCall :: Parser Ast
parseFuncCall =
    parseVar >>= \name ->
        parseSpaces *> parseChar '(' *> parseFuncCallContent
        name <* parseChar ')'

parseDefineSet :: String -> Parser Ast
parseDefineSet name = Parser $ \str ->
    case runParser parseCallOr str of
        Just (ast, rest) -> Just (Define name ast, rest)
        Nothing -> Nothing

parseDefine :: Parser Ast
parseDefine =
    parseVar >>= \name ->
        parseSpaces *> parseChar '=' *> parseSpaces *> parseDefineSet name

parseOr :: Parser Ast
parseOr = parseInt <* parseSep
      <|> parseBool <* parseSep
      <|> parseSymbol <* parseSep
      <|> parseString <* parseSep
      <|> parseFunc
      <|> parseIf
      <|> parseFuncCall <* parseSep
      <|> parseDefine <* parseSep

parseCallOr :: Parser Ast
parseCallOr = parseSpaces *> parseInt <* parseCallSep
      <|> parseSpaces *> parseBool <* parseCallSep
      <|> parseSpaces *> parseIf <* parseCallSep
      <|> parseSpaces *> parseFuncCall <* parseCallSep
      <|> parseSpaces *> parseSymbol <* parseCallSep
      <|> parseSpaces *> parseString <* parseCallSep

parseCallSep :: Parser String
parseCallSep = parseSpaces <* parseChar ',' <* parseSpaces
      <|> parseSpaces

parseListArgsCall :: Parser [Ast]
parseListArgsCall = Parser $ \str ->
    case str of
        [] -> Nothing
        (')':xs) -> Just ([], ')' : xs)
        _ ->
            case runParser parseCallOr str of
                Just (ast, rest) ->
                    case runParser parseListArgsCall rest of
                        Just (asts, rest1) -> Just (ast : asts, rest1)
                        Nothing -> Nothing
                Nothing -> Nothing

parseListCall :: Parser [Ast]
parseListCall = Parser $ \str ->
    case str of
        [] -> Nothing
        (')':xs) -> Just ([], ')' : xs)
        _ ->
            case runParser parseListArgsCall str of
                Just (args, rest) -> Just (args, rest)
                Nothing -> Nothing

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