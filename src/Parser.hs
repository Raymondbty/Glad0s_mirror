{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Types
import Utils

parseAsBracket :: Int -> String -> Either String (String, String)
parseAsBracket 0 rest = Right ([], rest)
parseAsBracket _ [] = Left "Unexpected end of input"
parseAsBracket 1 ('}':xs) = Right ([], xs)
parseAsBracket i ('{':xs) = case parseAsBracket (i + 1) xs of
                                Right (str, rest) -> Right ('{' : str, rest)
                                Left err -> Left err
parseAsBracket i ('}':xs) = case parseAsBracket (i - 1) xs of
                                Right (str, rest) -> Right ('}' : str, rest)
                                Left err -> Left err
parseAsBracket i (x:xs) = case parseAsBracket i xs of
                                Right (str, rest) -> Right (x : str, rest)
                                Left err -> Left err

parseAsParent :: String -> Either String (String, String)
parseAsParent [] = Right ([], [])
parseAsParent (' ':xs) = parseAsParent xs
parseAsParent (x:xs)
    | x == '(' = Right ([], xs)
    | otherwise = case parseAsParent xs of
        Right (str, rest) -> Right (x : str, rest)
        Left err -> Left err

parseWord :: String -> Either String (String, String, Bool)
parseWord [] = Left "Empty input"
parseWord (' ':xs) = parseWord xs
parseWord (x:xs)
    | x == ')' = Right ([], xs, True)
    | x == ',' = Right ([], xs, False)
    | checkLetter x = case parseWord xs of
        Right (str, rest, res) -> Right (x : str, rest, res)
        Left err -> Left err
    | otherwise = Left "Invalid input"

parseArgs :: String -> Either String (String, String, Bool)
parseArgs [] = Left "Empty input"
parseArgs (' ':xs) = parseArgs xs
parseArgs (x:xs)
    | x == ')' = Right ([], xs, True)
    | x == ',' = Right ([], xs, False)
    | checkLetter x = case parseArgs xs of
        Right (str, rest, res) -> Right (x : str, rest, res)
        Left err -> Left err
    | checkNumber x = case parseArgs xs of
        Right (str, rest, res) -> Right (x : str, rest, res)
        Left err -> Left err
    | otherwise = Left "Invalid input"

parseString :: String -> Either String (String, String)
parseString [] = Left "Empty input"
parseString ('"':xs) = Right ([], xs)
parseString ('\\':'"':xs) = case parseString xs of
    Right (str, rest) -> Right ('"' : str, rest)
    Left err -> Left err
parseString (x:xs) = case parseString xs of
    Right (str, rest) -> Right (x : str, rest)
    Left err -> Left err

parseVariable :: String -> Either String (String, String)
parseVariable [] = Left "Empty input"
parseVariable (' ':xs) = parseVariable xs
parseVariable ('\t':xs) = parseVariable xs
parseVariable (x:xs)
    | x == '=' = Right ([], xs)
    | checkLetter x = case parseVariable xs of
        Right (str, rest) -> Right (x : str, rest)
        Left err -> Left err
    | otherwise = Left "Invalid input"

parseVariableInt :: String -> Either String (String, String)
parseVariableInt [] = Left "Empty input"
parseVariableInt (' ':xs) = parseVariableInt xs
parseVariableInt (x:xs)
    | x == ';' = Right ([], xs)
    | checkNumber x = case parseVariableInt xs of
        Right (str, rest) -> Right (x : str, rest)
        Left err -> Left err
    | otherwise = Left "Invalid input"

parseInt :: String -> Ast
parseInt str = IntLiteral ((read str) :: Int)

parseStrings :: [String] -> [Ast]
parseStrings [] = []
parseStrings (x:xs) = case parseTypes x of
    Right ast -> ast : (parseStrings xs)
    Left _ -> []

parseOp :: String -> Either String (Ast, String)
parseOp [] = Left "Empty input"
parseOp str = case parseAsParent str of
    Right ("print", rest) -> case parseParamsTwo rest of
        Right (args, rest1) -> case parseAst (args !! 0) of
            Right (ast, _) -> Right ((Print ast), rest1)
            Left err -> Left err
        Left err -> Left err
    Right (name, rest) -> case parseParamsTwo rest of
        Right (args, rest1) -> Right (Call name (parseStrings args), rest1)
        Left err -> Left err
    Left err -> Left err

parseNum :: String -> Either String (Ast, String)
parseNum [] = Left "Empty input"
parseNum str = case parseVariable str of
    Right (var, ('"':xs)) -> case parseString xs of
        Right (str1, rest2) -> Right (Define var (StringLiteral str1), rest2)
        Left err -> Left err
    Right (var, rest) -> case parseVariableInt rest of
        Right (numStr, rest1) -> Right (Define var (parseInt numStr), rest1)
        Left err -> Left err
    Left err -> Left err

parseParamsTwo :: String -> Either String ([String], String)
parseParamsTwo [] = Right ([], [])
parseParamsTwo str = case parseArgs str of
    Right (arg, rest, True) -> Right ([arg], rest)
    Right (arg, rest, False) -> case parseParamsTwo rest of
        Right (args, rest1) -> Right (arg : args, rest1)
        Left err -> Left err
    Left err -> Left err

parseParams :: String -> Either String ([String], String)
parseParams [] = Right ([], [])
parseParams str = case parseWord str of
    Right (arg, rest, True) -> Right ([arg], rest)
    Right (arg, rest, False) -> case parseParams rest of
        Right (args, rest1) -> Right (arg : args, rest1)
        Left err -> Left err
    Left err -> Left err

parseFunc :: String -> Either String (Ast, String)
parseFunc [] = Left "Empty input"
parseFunc str = case parseAsParent str of
    Right (name, rest) -> case parseParams rest of
        Right (args, rest1) -> case firstBracket rest1 of
            Right rest2 -> case parseAsBracket 1 rest2 of
                Right (str2, rest3) -> Right (Func name (parse str2), rest3)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

parseTypes :: String -> Either String Ast
parseTypes [] = Left "Empty input"
parseTypes ('"':xs) = case parseString xs of
    Right (str, _) -> Right (StringLiteral str)
    Left err -> Left err
parseTypes str
    | checkString str = Right (Symbol str)
    | checkNumbers str = Right (parseInt str)
    | otherwise = Left "Invalid input"

parseAst :: String -> Either String (Ast, String)
parseAst rest = case parseNum rest of
        Right (ast, rest1) -> Right (ast, rest1)
        Left _ -> case parseTypes rest of
            Right ast -> Right (ast, rest)
            Left _ -> case parseOp rest of
                Right (ast, rest2) -> Right (ast, rest2)
                Left err -> Left err

parse :: String -> [Ast]
parse [] = []
parse (';':xs) = parse xs
parse (' ':xs) = parse xs
parse ('\t':xs) = parse xs
parse ('\r':xs) = parse xs
parse ('\n':xs) = parse xs
parse str = case firstWord str of
    ("func", rest) -> case parseFunc rest of
        Right (ast, rest1) -> ast : parse rest1
        Left _ -> []
    (x, xs) -> case parseAst (x ++ xs) of
        Right (ast, rest1) -> ast : parse rest1
        Left _ -> []
