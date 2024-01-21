{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Disassembler.hs
-}

module Disassembler (disassemble) where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import System.Exit

continueReading :: String -> [Word8] -> Maybe String
continueReading str xs =
    case getAssembler xs of
        Just [] -> Just $ str
        Just code -> Just $ str ++ "\n" ++ code
        Nothing -> Nothing

bytesToInt :: Word8 -> Word8 -> Word8 -> Word8 -> Int
bytesToInt b1 b2 b3 b4 = (fromIntegral b1)
                       + (fromIntegral b2 `shiftL` 8)
                       + (fromIntegral b3 `shiftL` 16)
                       + (fromIntegral b4 `shiftL` 24)

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
readCall (0x07:xs) = continueReading "Push Fact\nCall" xs
readCall _ = Nothing

getAssembler :: [Word8] -> Maybe String
getAssembler [] = Just []
getAssembler (0x00:xs) = readInstr "Push" xs
getAssembler (0x01:xs) = readCall xs
getAssembler (0x02:xs) = continueReading "Ret" xs
getAssembler (0x03:xs) = readInstr "JumpIfFalse" xs
getAssembler (0x04:xs) = readInstr "Jump" xs
getAssembler _ = Nothing

checkHeader :: [Word8] -> Maybe [Word8]
checkHeader (0x2F:0x47:0x4C:0x61:0x44:0x4F:0x53:xs) = Just xs
checkHeader _ = Nothing

disassemble :: String -> IO ()
disassemble file = do
    result <- try (BS.readFile file) :: IO (Either SomeException BS.ByteString)
    case result of
        Left e ->
            putStrLn ("Exception: " ++ show e) >> exitWith (ExitFailure 84)
        Right byteStr ->
            let binary = BS.unpack byteStr in
            case checkHeader binary of
                Just binaryRest ->
                    case getAssembler binaryRest of
                        Just code -> putStrLn code
                        Nothing -> putStrLn $ "Error: binary corrupted"
                Nothing -> putStrLn $ "Error: file is not a GLaDOS binary"
