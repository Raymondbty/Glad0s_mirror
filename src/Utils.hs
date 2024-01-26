{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Utils.hs
-}

module Utils (intToBytes, bytesToInt, firstWord,
checkNumber, checkLetter,checkNumbers, checkString,
firstBracket) where

import Data.Bits
import Data.Word

intToBytes :: Int -> [Word8]
intToBytes i = map fromIntegral [i, i `shiftR` 8, i `shiftR` 16, i `shiftR` 24]

bytesToInt :: Word8 -> Word8 -> Word8 -> Word8 -> Int
bytesToInt b1 b2 b3 b4 = (fromIntegral b1)
                       + (fromIntegral b2 `shiftL` 8)
                       + (fromIntegral b3 `shiftL` 16)
                       + (fromIntegral b4 `shiftL` 24)

firstWord :: String -> (String, String)
firstWord [] = ([], [])
firstWord (x:xs)
    | x `elem` " \t\r\n" = ([], xs)
    | otherwise = let (str, rest) = firstWord xs in
                  (x : str, rest)

firstBracket :: String -> Either String String
firstBracket [] = Right []
firstBracket (' ':xs) = firstBracket xs
firstBracket ('{':xs) = Right xs
firstBracket _ = Left "No opening bracket found"

checkNumber :: Char -> Bool
checkNumber x = (x >= '0' && x <= '9')

checkLetter :: Char -> Bool
checkLetter x = (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x == '_')

checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = (checkLetter x) && (checkString xs)

checkNumbers :: String -> Bool
checkNumbers [] = True
checkNumbers (x:xs) = (checkNumber x) && (checkNumbers xs)