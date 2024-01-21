{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- File.hs
-}

module File (readBinary) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Word
import System.Exit

checkHeader :: [Word8] -> Maybe [Word8]
checkHeader (0x2F:0x47:0x4C:0x61:0x44:0x4F:0x53:xs) = Just xs
checkHeader _ = Nothing

readBinary :: String -> IO (Either String [Word8])
readBinary file = do
    result <- try (BS.readFile file) :: IO (Either SomeException BS.ByteString)
    case result of
        Left e ->
            putStrLn ("Exception: " ++ show e) >> exitWith (ExitFailure 84)
        Right byteStr ->
            case checkHeader $ BS.unpack byteStr of
                Just binary -> return $ Right binary
                Nothing -> return $ Left "Error: file is not a GLaDOS binary"
