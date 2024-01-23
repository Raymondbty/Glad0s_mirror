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

parseAsBracket :: Int -> String -> Maybe (String, String)
parseAsBracket 0 rest = Just ([], rest)
parseAsBracket _ [] = Nothing
parseAsBracket i ('{':xs) = case parseAsBracket (i + 1) xs of
                                Just (str, rest) -> Just ('{' : str, rest)
                                Nothing -> Nothing
parseAsBracket i ('}':xs) = case parseAsBracket (i - 1) xs of
                                Just (str, rest) -> Just ('}' : str, rest)
                                Nothing -> Nothing
parseAsBracket i (x:xs) = case parseAsBracket i xs of
                                Just (str, rest) -> Just (x : str, rest)
                                Nothing -> Nothing

firstBracket :: String -> Maybe String
firstBracket [] = Just []
firstBracket (' ':xs) = firstBracket xs
firstBracket ('{':xs) = Just xs
firstBracket _ = Nothing

parseAsParent :: String -> Maybe (String, String)
parseAsParent [] = Just ([], [])
parseAsParent (' ': xs) = parseAsParent xs
parseAsParent (x:xs)
    | x == '(' = Just ([], xs)
    | otherwise = case parseAsParent xs of
        Just (str, rest) -> Just (x : str, rest)
        Nothing -> Nothing

checkLetter :: Char -> Bool
checkLetter x = (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z' )

checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = (checkLetter x) && (checkString xs)

checkNumbers :: String -> Bool
checkNumbers [] = True
checkNumbers (x:xs) = (checkNumber x) && (checkNumbers xs)

parseWord :: String -> Maybe (String, String, Bool)
parseWord [] = Nothing
parseWord (' ': xs) = parseWord xs
parseWord (x:xs)
    | x == ')' = Just ([], xs, True)
    | x == ',' = Just ([], xs, False)
    | checkLetter x =  case parseWord xs of
        Just (str, rest, res) -> Just (x : str, rest, res)
        Nothing -> Nothing
    | otherwise = Nothing

parseArgs :: String -> Maybe (String, String, Bool)
parseArgs [] = Nothing
parseArgs (' ': xs) = parseArgs xs
parseArgs (x:xs)
    | x == ')' = Just ([], xs, True)
    | x == ',' = Just ([], xs, False)
    | checkLetter x =  case parseArgs xs of
        Just (str, rest, res) -> Just (x : str, rest, res)
        Nothing -> Nothing
    | checkNumber x =  case parseArgs xs of
    Just (str, rest, res) -> Just (x : str, rest, res)
    Nothing -> Nothing
    | otherwise = Nothing

checkNumber :: Char -> Bool
checkNumber x = (x >= '0' && x <= '9')

parseString :: String -> Maybe (String, String)
parseString [] = Nothing
parseString ('"':xs) = Just ([], xs)
parseString ('\\':'"':xs) = case parseString xs of
    Just (str, rest) -> Just ('"' : str, rest)
    Nothing -> Nothing
parseString (x:xs) = case parseString xs of
    Just (str, rest) -> Just (x : str, rest)
    Nothing -> Nothing

parseVariable :: String -> Maybe (String, String)
parseVariable [] = Nothing
parseVariable (' ': xs) = parseVariable xs
parseVariable ('\t': xs) = parseVariable xs
parseVariable (x:xs)
    | x == '=' = Just ([], xs)
    | checkLetter x = case parseVariable xs of
        Just (str, rest) -> Just (x : str, rest)
        Nothing -> Nothing
    | otherwise = Nothing

parseVariableInt :: String -> Maybe (String, String)
parseVariableInt [] = Nothing
parseVariableInt (' ': xs) = parseVariableInt xs
parseVariableInt (x:xs)
    | x == ';' = Just ([], xs)
    | checkNumber x = case parseVariableInt xs of
        Just (str, rest) -> Just (x : str, rest)
        Nothing -> Nothing
    | otherwise = Nothing

parseInt :: String -> Ast
parseInt str = IntLiteral ((read str) :: Int)

parseStrings :: [String] -> [Ast]
parseStrings [] = []
parseStrings (x:xs) = case parseTypes x of
    Just ast -> ast : (parseStrings xs)
    Nothing -> []

parseOp :: String -> Maybe (Ast, String)
parseOp [] = Nothing
parseOp str = case parseAsParent str of
    Just (name, rest) -> case parseParamsTwo rest of
        Just (args, rest1) -> Just ((Call name (parseStrings args)), rest1)
        Nothing -> Nothing
    Nothing -> Nothing

parseNum :: String -> Maybe (Ast, String)
parseNum [] = Nothing
parseNum str = case parseVariable str of
    Just (var, ('"':xs)) -> case parseString xs of
        Just (str1, rest2) -> Just (Define var (StringLiteral str1), rest2)
        Nothing -> Nothing
    Just (var, rest) -> case parseVariableInt rest of
        Just (numStr, rest1) -> Just (Define var (parseInt numStr), rest1)
        Nothing -> Nothing
    Nothing -> Nothing

parseParamsTwo :: String -> Maybe ([String], String)
parseParamsTwo [] = Just([], [])
parseParamsTwo str = case parseArgs str of
    Just (arg, rest, True) -> Just ([arg], rest)
    Just (arg, rest, False) -> case parseParamsTwo rest of
        Just (args, rest1) -> Just (arg : args, rest1)
        Nothing -> Nothing
    Nothing -> Nothing

parseParams :: String -> Maybe ([String], String)
parseParams [] = Just([], [])
parseParams str = case parseWord str of
    Just (arg, rest, True) -> Just ([arg], rest)
    Just (arg, rest, False) -> case parseParams rest of
        Just (args, rest1) -> Just (arg : args, rest1)
        Nothing -> Nothing
    Nothing -> Nothing

parseFunc :: String -> Maybe (Ast, String)
parseFunc [] = Nothing
parseFunc str = case parseAsParent str of
    Just (name, rest) -> case parseParams rest of
        Just (args, rest1) -> case firstBracket rest1 of
            Just rest2 -> case parseAsBracket 1 rest2 of
                Just (str2, rest3) -> Just (Call2 name args (parse str2), rest3)
                Nothing -> Nothing
            Nothing -> Nothing
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
                    Just (ast, rest1) -> ast : (parse rest1)
                    Nothing -> []
                (x, xs) -> case parseNum (x ++ xs) of
                    Just (ast, rest1) -> ast : (parse rest1)
                    Nothing -> []
