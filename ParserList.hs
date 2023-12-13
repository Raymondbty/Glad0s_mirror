import Control.Applicative
import Data.Char (isDigit, isSpace)

-- ParseurList.hs
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Instance Functor
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> fmap (\(a, rest) -> (f a, rest)) (p input)

-- Instance Applicative
instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input ->
        case p1 input of
            Just (f, rest) -> fmap (\(a, rest') -> (f a, rest')) (p2 rest)
            Nothing -> Nothing

-- Instance Alternative
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- Apply parsing and print
parse :: Show a => Parser a -> String -> IO ()
parse p input = do
    case runParser p input of
        Just (result, rest) -> do
            putStrLn $ "Parsing succeeded. Result: " ++ show result
            putStrLn $ "Remaining input: " ++ rest
        Nothing -> putStrLn "Parsing failed"

-- Parse Int
parseInt :: Parser Int
parseInt = read <$> some (satisfy isDigit)

-- Satisfy : condition for parsing authorization
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f
  where
    f (y:ys) | pred y    = Just (y, ys)
            | otherwise = Nothing
    f []                 = Nothing

-- Parse a list
parseList :: Parser a -> Parser [a]
parseList p = char '(' *> spaces *> elements <* char ')' <* spaces
  where
    elements = (:) <$> p <*> many (spaces *> char ',' *> spaces *> p)

-- Parse a char c
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parseur pour les espaces
spaces :: Parser String
spaces = many (satisfy isSpace)

-- applycation and tests
main :: IO ()
main = do
    -- test list from exercice (valid)
    parse (parseList parseInt) "(1, 2, 3, 5, 7, 11, 13, 17)"

    -- test empty list
    parse (parseList parseInt) "()"

    -- test without spaces
    parse (parseList parseInt) "(1,2,3,4,5)"

    -- test with bigger nb
    parse (parseList parseInt) "(10 , 20 , 30 , 40 , 50)"

    -- test with multiples spaces
    parse (parseList parseInt) "(2 , 4 , 6 , 8)"

    -- test with no closed ()
    parse (parseList parseInt) "(1, 2, 3, 4, 5"