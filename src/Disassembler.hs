{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Disassembler.hs
-}

module Disassembler (disassemble) where

import Data.Word
import File
import System.Exit
import Utils

continueReading :: String -> [Word8] -> Maybe String
continueReading str xs =
    case getAssembler xs of
        Just [] -> Just $ str
        Just code -> Just $ str ++ "\n" ++ code
        Nothing -> Nothing

readInstr :: String -> [Word8] -> Maybe String
readInstr instr (b1:b2:b3:b4:xs) =
    continueReading (instr ++ " " ++ (show $ bytesToInt b1 b2 b3 b4)) xs
readInstr _ _ = Nothing

readCall :: [Word8] -> Maybe String
readCall (0x00:xs) = continueReading "Push Add\nCall" xs
readCall (0x01:xs) = continueReading "Push Sub\nCall" xs
readCall (0x02:xs) = continueReading "Push Mul\nCall" xs
readCall (0x03:xs) = continueReading "Push Div\nCall" xs
readCall (0x04:xs) = continueReading "Push Mod\nCall" xs
readCall (0x05:xs) = continueReading "Push Eq\nCall" xs
readCall (0x06:xs) = continueReading "Push Less\nCall" xs
readCall _ = Nothing

getAssembler :: [Word8] -> Maybe String
getAssembler [] = Just []
getAssembler (0x00:xs) = readInstr "Push" xs
getAssembler (0x01:xs) = readCall xs
getAssembler (0x02:xs) = continueReading "Ret" xs
getAssembler (0x03:xs) = readInstr "JumpIfFalse" xs
getAssembler (0x04:xs) = readInstr "Jump" xs
getAssembler _ = Nothing

disassemble :: String -> IO ()
disassemble file = do
    binaryFile <- readBinary file
    case binaryFile of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right binary ->
            case getAssembler binary of
                Just code -> putStrLn code
                Nothing -> putStrLn "Error: binary corrupted" >>
                    exitWith (ExitFailure 84)
