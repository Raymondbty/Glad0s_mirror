{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Compiler.hs
-}

module Compiler (compile, convAst, loopAst) where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import System.Exit
import Types

calcJump :: [Word8] -> Int
calcJump _ = 0

intToBytes :: Int -> [Word8]
intToBytes i = map fromIntegral [i, i `shiftR` 8, i `shiftR` 16, i `shiftR` 24]

convAst :: Ast -> [Word8]
convAst (Call "+" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x00]
convAst (Call "-" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x01]
convAst (Call "*" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x02]
convAst (Call "/" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x03]
convAst (Call "div" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x03]
convAst (Call "%" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x04]
convAst (Call "mod" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x04]
convAst (Call "?" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x05]
convAst (Call "<" (ast1:ast2:_)) =
  (loopAst [ast2]) ++ (loopAst [ast1]) ++ [0x01, 0x06]
convAst (Call "!" (ast:_)) =
  (loopAst [ast]) ++ [0x01, 0x07]
convAst (Call "if" (ast1:ast2:ast3:_)) =
  let cond = loopAst [ast1]
      trueCond = loopAst [ast2] ++ [0x04] ++ (intToBytes $ calcJump falseCond)
      falseCond = loopAst [ast3] in
  cond ++ [0x03] ++ (intToBytes $ calcJump trueCond) ++ trueCond ++ falseCond
convAst (IntLiteral i) = 0x00 : (intToBytes i)
convAst (BoolLiteral True) = 0x00 : (intToBytes 1)
convAst (BoolLiteral False) = 0x00 : (intToBytes 0)
convAst _ = []

loopAst :: [Ast] -> [Word8]
loopAst [] = []
loopAst (x:xs) = (convAst x) ++ (loopAst xs)

getHeader :: [Word8]
getHeader = [0x2F, 0x47, 0x4C, 0x61, 0x44, 0x4F, 0x53]

compile :: [Ast] -> String -> IO ()
compile [] _ = return ()
compile (x:_) path = do
  result <- try (BS.writeFile path binary) :: IO (Either SomeException ())
  case result of
    Left e ->
      (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
    Right () -> return ()
  where
    binary = BS.pack $ (getHeader ++ loopAst [x]) ++ [0x02]
