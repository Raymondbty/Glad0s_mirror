{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Utils.hs
-}

module Utils (intToBytes, bytesToInt) where

import Data.Bits
import Data.Word

intToBytes :: Int -> [Word8]
intToBytes i = map fromIntegral [i, i `shiftR` 8, i `shiftR` 16, i `shiftR` 24]

bytesToInt :: Word8 -> Word8 -> Word8 -> Word8 -> Int
bytesToInt b1 b2 b3 b4 = (fromIntegral b1)
                       + (fromIntegral b2 `shiftL` 8)
                       + (fromIntegral b3 `shiftL` 16)
                       + (fromIntegral b4 `shiftL` 24)
