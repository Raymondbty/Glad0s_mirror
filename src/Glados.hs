{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS.hs
-}

module Glados (start) where

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving Show

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol expr) = Just expr
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt expr) = Just expr
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList expr) = Just expr
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SInt expr) = Just $ "a Number " ++ show expr
printTree (SSymbol expr) = Just $ "a Symbol '" ++ expr ++ "'"
printTree (SList []) = Just "an empty List"
printTree (SList (x:xs)) = case printTree x of
                                Just expr -> Just $ "(a List with " ++ expr ++ " followed by " ++ printTreeList xs ++ ")"
                                Nothing -> Just $ "a List with nothing followed by " ++ printTreeList xs

printTreeList :: [SExpr] -> String
printTreeList [] = "nothing"
printTreeList [x] = case printTree x of
                        Just str -> str
                        Nothing -> "nothing"
printTreeList (x:xs) = case printTree x of
                           Just str -> str ++ ", " ++ printTreeList xs
                           Nothing -> printTreeList xs

data Ast = Define Ast Ast
         | Call String [Ast]
         | IntLiteral Int
         | StringLiteral String
         deriving Show

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt i) = Just $ IntLiteral i
sexprToAST (SSymbol i) = Just $ StringLiteral i
sexprToAST (SList [SSymbol "define", SSymbol symbol, SInt i]) = Just $ Define (StringLiteral symbol) (IntLiteral i)
sexprToAST (SList [SSymbol "define", SSymbol symbol, SSymbol s]) = Just $ Define (StringLiteral symbol) (StringLiteral s)
sexprToAST _ = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (Call "+" [IntLiteral x, IntLiteral y]) = Just $ IntLiteral $ x + y
evalAST (Call "-" [IntLiteral x, IntLiteral y]) = Just $ IntLiteral $ x - y
evalAST (Call "*" [IntLiteral x, IntLiteral y]) = Just $ IntLiteral $ x * y
evalAST (Call "/" [IntLiteral _, IntLiteral 0]) = Just $ IntLiteral $ 0
evalAST (Call "/" [IntLiteral x, IntLiteral y]) = Just $ IntLiteral $ x `div` y
evalAST (Call "%" [IntLiteral _, IntLiteral 0]) = Just $ IntLiteral $ 0
evalAST (Call "%" [IntLiteral x, IntLiteral y]) = Just $ IntLiteral $ x `mod` y
evalAST _ = Nothing

type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar c (x:xs) | x == c = Just (c, xs)
                   | otherwise = Nothing
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar list (x:xs) | x `elem` list = Just (x, xs)
                         | otherwise = Nothing
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 list = case p1 list of
                        Just p -> Just p
                        Nothing -> p2 list

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 list = case p1 list of
                        Just (c1, list1) -> case p2 list1 of
                                                Just (c2, list2) -> Just ((c1, c2), list2)
                                                Nothing -> Nothing
                        Nothing -> Nothing

start :: IO ()
start = do
    let expr = SList [SInt 42, SInt 42, SSymbol "qzf qzf", SList [SInt 42, SInt 42, SSymbol "qzf qzf"]]
    let mast = Call "/" [IntLiteral 42, IntLiteral 0]
    case printTree expr of
        Just str -> putStrLn str
        Nothing -> putStrLn "Nothing to print"
    case evalAST mast of
        Just ast -> putStrLn $ show ast
        Nothing -> putStrLn "mast is Nothing"
    putStrLn $ show $ parseChar 'a' "abcd"
    putStrLn $ show $ parseChar 'z' "abcd"
    putStrLn $ show $ parseChar 'b' "abcd"
    putStrLn $ show $ parseChar 'a' "aaaa"
    putStrLn $ show $ parseAnyChar "bca" "abcd"
    putStrLn $ show $ parseAnyChar "xyz" "abcd"
    putStrLn $ show $ parseAnyChar "bca" "cdef"
    putStrLn $ show $ parseOr (parseChar 'a') (parseChar 'b') "abcd"
    putStrLn $ show $ parseOr (parseChar 'a') (parseChar 'b') "bcda"
    putStrLn $ show $ parseOr (parseChar 'a') (parseChar 'b') "xyz"
    putStrLn $ show $ parseAnd (parseChar 'a') (parseChar 'b') "abcd"
    putStrLn $ show $ parseAnd (parseChar 'a') (parseChar 'b') "bcda"
    putStrLn $ show $ parseAnd (parseChar 'a') (parseChar 'b') "acd"
