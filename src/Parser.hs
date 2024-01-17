{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (isNumber, isStringNumber, firstWord, parseList, parseCall, parseString, parseIntSym, parse) where

import Types

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

isStringNumber :: String -> Bool
isStringNumber [] = False
isStringNumber [x] = isNumber x
isStringNumber (x:xs) = isNumber x && isStringNumber xs

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs)
    | x `elem` " \t\r\n" = ([], xs)
    | otherwise = let (str, rest) = firstWord xs in
                  (x : str, rest)

parseList :: (Int, String) -> (String, String)
parseList (0, rest) = ([], rest)
parseList (1, (')':xs)) = ([], xs)
parseList (i, (')':xs)) = let (list, rest) = parseList (i - 1, xs) in
                            (')' : list, rest)
parseList (i, ('(':xs)) = let (list, rest) = parseList (i + 1, xs) in
                           ('(' : list, rest)
parseList (i, (x:xs)) = let (list, rest) = parseList (i, xs) in
                        (x : list, rest)
parseList (_, []) = ([], [])

parseDefine :: String -> Maybe Ast
parseDefine args = case parse args of
                    [(StringLiteral var), ast] -> Just $ Define var ast
                    [(Symbol var), ast] -> Just $ Define var ast
                    _ -> Nothing

symbolToString :: [Ast] -> Maybe [String]
symbolToString [] = Just []
symbolToString (Symbol str:xs) = case symbolToString xs of
                                    Nothing -> Nothing
                                    Just strs -> Just $ str : strs
symbolToString _ = Nothing

parseLambda :: String -> Maybe Ast
parseLambda (' ':xs) = parseLambda xs
parseLambda ('\t':xs) = parseLambda xs
parseLambda ('\r':xs) = parseLambda xs
parseLambda ('\n':xs) = parseLambda xs
parseLambda ('(':xs) = let (list, rest) = parseList (1, xs) in
                       case symbolToString $ parse list of
                        Nothing -> Nothing
                        Just strs -> case parse rest of
                                        [] -> Nothing
                                        (x:_) -> Just $ (Lambda strs x)
parseLambda _ = Nothing

parseCall :: String -> (Maybe Ast, String)
parseCall str = let (list, rest) = parseList (1, str) in
                case firstWord list of
                    ([], _) -> (Nothing, rest)
                    ("define", args) -> (parseDefine args, rest)
                    ("lambda", args) -> (parseLambda args, rest)
                    (word, args) -> (Just $ Call word (parse args), rest)

parseString :: String -> (String, String)
parseString [] = ([], [])
parseString ('"':xs) = ([], xs)
parseString ('\\':'"':xs) = let (str, rest) = (parseString xs) in
                            ('"' : str, rest)
parseString (x:xs) = let (str, rest) = (parseString xs) in
                     (x : str, rest)

parseIntSym :: String -> Ast
parseIntSym ('-':xs) | isStringNumber xs =
    IntLiteral (((read xs) :: Int) * (-1))
parseIntSym str      | isStringNumber str =
    IntLiteral ((read str) :: Int)
parseIntSym str      = Symbol str

parse :: String -> [Ast]
parse [] = []
parse (' ':xs) = parse xs
parse ('\t':xs) = parse xs
parse ('\r':xs) = parse xs
parse ('\n':xs) = parse xs
parse ('(':xs) = case parseCall xs of
                    (Just ast, rest) -> ast : (parse rest)
                    (Nothing, rest) -> (parse rest)
parse ('"':xs) = case parseString xs of
                    ([], _) -> []
                    (str, rest) -> (StringLiteral str) : (parse rest)
parse ('#':'t':xs) = (BoolLiteral True) : (parse xs)
parse ('#':'f':xs) = (BoolLiteral False) : (parse xs)
parse str = case firstWord str of
                ([], _) -> []
                (word, rest) -> (parseIntSym word) : (parse rest)
