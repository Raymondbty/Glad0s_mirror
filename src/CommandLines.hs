{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- CommandLines.hs
-}

module CommandLines (quitCommand, helpCommand, manCommand) where

quitCommand :: IO ()
quitCommand = putStrLn "Quitting Glados."

manCommand :: IO ()
manCommand = do
    putStrLn "Welcome to the manual !\n"
    putStrLn "---------------------------------------------"
    putStrLn "Add:"
    putStrLn "  example -> (+ 1 2)"
    putStrLn "---------------------------------------------"
    putStrLn "Sub:"
    putStrLn "  example -> (- 2 1)"
    putStrLn "---------------------------------------------"
    putStrLn "Mul:"
    putStrLn "  example -> (* 2 2)"
    putStrLn "---------------------------------------------"
    putStrLn "Div:"
    putStrLn "  example -> (/ 6 2)"
    putStrLn "  example -> (div 6 2)"
    putStrLn "---------------------------------------------"
    putStrLn "Eq:"
    putStrLn "  example -> (eq? \"test\" \"test\")"
    putStrLn "---------------------------------------------"
    putStrLn "Cond:"
    putStrLn "  example -> (if (eq? 5 4) (+ 6 6) (- 7 1))"
    putStrLn "---------------------------------------------"

helpCommand :: IO ()
helpCommand = do
    putStrLn "Welcome to Glados !\n"
    putStrLn "Commands:"
    putStrLn "  !quit - Quit the program."
    putStrLn "  !man  - Display the manual."
    putStrLn "  !help - Display this help message."
