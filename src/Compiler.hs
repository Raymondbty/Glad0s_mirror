{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Compiler.hs
-}

module Compiler (compile) where

import Control.Exception
import Print
import System.Exit
import Types

compile :: [Ast] -> String -> IO ()
compile [] _ = return ()
compile (x:_) path = do
    result <- try (writeFile path (prettyPrint x)) :: IO (Either SomeException ())
    case result of
        Left e -> (putStrLn ("Exception: " ++ (show e))) >> (exitWith (ExitFailure 84))
        Right () -> return ()
