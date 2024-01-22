module SnakeLanguage where

data SnakeAST = Hiss SnakeAST
              | Slither String SnakeAST
              | NoHissing
              | Print String
              | If SnakeAST SnakeAST SnakeAST
              | Range SnakeAST SnakeAST
              | Call String [SnakeAST]
              | Literal Int
              deriving Show

snakeLexer :: String -> [String]
snakeLexer = words . map (\case
    '(' -> ' ' 
    ')' -> ' '
    ',' -> ' '
    c   -> c)

snakeParser :: [String] -> [SnakeAST]
snakeParser = map parseToken

parseToken :: String -> SnakeAST
parseToken "hiss" = Hiss NoHissing
parseToken "slither" = Slither "" NoHissing
parseToken "nohissing" = NoHissing
parseToken "print" = Print ""
parseToken "if" = If NoHissing NoHissing NoHissing
parseToken "range" = Range NoHissing NoHissing
parseToken word
    | all (\c -> c >= '0' && c <= '9') word = Literal (read word :: Int)
    | otherwise = Call word []

snakeInterpreter :: SnakeAST -> IO ()
snakeInterpreter = \case
    Hiss action -> snakeInterpreter action
    Slither var action -> do
        putStr "Slithering through "
        putStr var
        putStrLn "..."
        snakeInterpreter action
    NoHissing -> putStrLn "No hissing today."
    Print message -> putStrLn $ "Printing: " ++ message
    If cond trueBranch falseBranch -> do
        putStrLn "Checking condition..."
        case cond of
            NoHissing -> putStrLn "Condition is false. No hissing."
            _ -> do
                putStrLn "Condition is true. Hissing..."
                snakeInterpreter trueBranch
        snakeInterpreter falseBranch
    Range start end -> do
        putStr "Slithering in range from "
        putStr (show start)
        putStr " to "
        putStr (show end)
        putStrLn "..."
    Call func args -> do
        putStr "Calling "
        putStr func
        putStr " with arguments: "
        putStrLn (show args)
    Literal value -> putStrLn $ "Literal value: " ++ show value

-- example, may change, to try out
snakeCode :: String
snakeCode = "hiss (slither 'i' (range 1 5) (print \"Sssssss!\"))"

main :: IO ()
main = snakeInterpreter . head . snakeParser . snakeLexer $ snakeCode
