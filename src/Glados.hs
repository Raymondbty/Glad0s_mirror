{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Glados.hs
-}

module Glados (start) where

import CommandLines
import Compiler
import Disassembler
import Eval (evalAST)
import Parser (parse)
import Print
import System.Environment
import System.IO
import Types
import VM

printFuncRes :: [Ast] -> IO ()
printFuncRes [] = return ()
printFuncRes (x:xs) = (putStrLn $ prettyPrintString x) >> printFuncRes xs

run :: [Ast] -> [Env] -> IO ()
run [] _ = return ()
run (x:xs) env = case evalAST 1 x env of
    Left err -> putStrLn ("Exception: " ++ err ++ ": " ++ (prettyPrint x))
    Right (FuncRes asts, env1) -> (printFuncRes asts) >> run xs env1
    Right (IfRes asts, env1) -> (printFuncRes asts) >> run xs env1
    Right (Print asts, env1) ->
        (putStrLn $ prettyPrintString (Print asts)) >> run xs env1
    Right (_, env1) -> run xs env1

getInput :: IO (String)
getInput = isEOF >>= \eof ->
    case eof of
        True -> return []
        False -> getLine >>= \line ->
            getInput >>= \rest ->
            return $ line ++ rest

start :: IO ()
start = getArgs >>= \args ->
    case args of
        ("--compile":file:_) -> startInterpreter $ Just file
        ("--compile":_)      -> putStrLn "--compile argument needs a file"
        ("--disassemble":file:_) -> disassemble file
        ("--disassemble":_)      -> putStrLn "--disassemble needs a file"
        ("--vm":file:_)      -> startVM file
        ("--vm":_)           -> putStrLn "--vm argument needs a file"
        ("--man":_)          -> manCommand
        ("--help":_)         -> helpCommand
        _                    -> startInterpreter Nothing

startInterpreter :: Maybe String -> IO ()
startInterpreter file = getInput >>= \input ->
    case runParser parse input of
        Just (asts, []) ->
            case file of
                Just path -> compile asts path
                Nothing -> run asts []
        _ -> putStrLn $ "Parser error"
