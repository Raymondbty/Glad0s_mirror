{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Disassembler.hs
-}

module Disassembler (disassemble) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Word
import System.Exit

getAssembler :: [Word8] -> Either String String
getAssembler _ = Left "str"

disassemble :: String -> IO ()
disassemble file = do
    result <- try (BS.readFile file) :: IO (Either SomeException BS.ByteString)
    case result of
        Left e ->
            putStrLn ("Exception: " ++ show e) >> exitWith (ExitFailure 84)
        Right binary -> case getAssembler $ BS.unpack binary of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right str -> putStrLn str
