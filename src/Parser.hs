{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parser.hs
-}

module Parser (parse) where

import Ast
import Data.Char

isStringNumber :: String -> Bool
isStringNumber [] = False
isStringNumber [x] = isNumber x
isStringNumber (x:xs) = isNumber x && isStringNumber xs

removeLastElem :: String -> String
removeLastElem [] = []
removeLastElem [_] = []
removeLastElem (x:xs) = x : removeLastElem xs

parseArgs :: [String] -> [Ast]
parseArgs [] = []
parseArgs (x:xs) = case parse x of
                        Just ast -> ast : (parseArgs xs)
                        Nothing -> (parseArgs xs)

parseCall :: String -> Maybe Ast
parseCall str = case (words str) of
                    [] -> Nothing
                    (word:rest) -> Just $ Call word (parseArgs rest)

parse :: String -> Maybe Ast
parse [] = Nothing
parse (x:xs) | x == '(' && null xs == False && (head $ reverse xs) == ')' = parseCall $ removeLastElem xs
             | isStringNumber (x : xs) = Just $ IntLiteral (read (x : xs) :: Int)
             | otherwise = Just $ StringLiteral (x : xs)
