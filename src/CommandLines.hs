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
manCommand = putStrLn $ "Welcome to the manual !\n"
                     ++ "---------------------------------------------"
                     ++ "Add:"
                     ++ "  example -> (+ 1 2)"
                     ++ "---------------------------------------------"
                     ++ "Sub:"
                     ++ "  example -> (- 2 1)"
                     ++ "---------------------------------------------"
                     ++ "Mul:"
                     ++ "  example -> (* 2 2)"
                     ++ "---------------------------------------------"
                     ++ "Div:"
                     ++ "  example -> (/ 6 2)"
                     ++ "  example -> (div 6 2)"
                     ++ "---------------------------------------------"
                     ++ "Eq:"
                     ++ "  example -> (eq? \"test\" \"test\")"
                     ++ "---------------------------------------------"
                     ++ "Cond:"
                     ++ "  example -> (if (eq? 5 4) (+ 6 6) (- 7 1))"
                     ++ "---------------------------------------------"
                     ++ "Def:"
                     ++ "  example -> (define test 5)"
                     ++ "  example -> (define \"test\" (+ 5 6))"
                     ++ "---------------------------------------------"

helpCommand :: IO ()
helpCommand = putStrLn $ "Welcome to Glados !\n"
                      ++ "Commands:"
                      ++ "  !quit - Quit the program."
                      ++ "  !man  - Display the manual."
                      ++ "  !help - Display this help message."
