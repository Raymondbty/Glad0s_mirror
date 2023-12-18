data Ast = Define { variable :: String, expression :: Ast }
         | IntegerLiteral Int
         | Symbol String
         | BooleanLiteral Bool
         | Call { functionName :: String, arguments :: [Ast] }
         deriving Show

evalAST :: Ast -> Maybe Ast
evalAST (IntegerLiteral n) = Just $ IntegerLiteral n
evalAST (Symbol _) = Nothing
evalAST (BooleanLiteral b) = Just $ BooleanLiteral b
evalAST (Define _ _) = Nothing

evalAST (Call "+" args) = do
  evaluatedArgs <- mapM evalAST args
  let result = sum [n | IntegerLiteral n <- evaluatedArgs]
  Just $ IntegerLiteral result

evalAST (Call "*" args) = do
  evaluatedArgs <- mapM evalAST args
  let result = product [n | IntegerLiteral n <- evaluatedArgs]
  Just $ IntegerLiteral result

evalAST (Call "/" args) = do
  evaluatedArgs <- mapM evalAST args
  let result = case evaluatedArgs of
                 [IntegerLiteral n1, IntegerLiteral n2] -> if n2 /= 0 then n1 `div` n2 else error "Division by zero"
                 _ -> error "Invalid arguments for division"
  Just $ IntegerLiteral result

evalAST (Call "-" [arg1, arg2]) = do
  evalArg1 <- evalAST arg1
  evalArg2 <- evalAST arg2
  case (evalArg1, evalArg2) of
    (IntegerLiteral n1, IntegerLiteral n2) -> Just $ IntegerLiteral (n1 - n2)
    _ -> Nothing

evalAST (Call "if" [condition, trueBranch, falseBranch]) = do
  evalCondition <- evalAST condition
  case evalCondition of
    BooleanLiteral True -> evalAST trueBranch
    BooleanLiteral False -> evalAST falseBranch
    _ -> Nothing

evalAST (Call _ _) = Nothing

main :: IO ()
main = do
  let example = Call "/" [Call "*" [Call "+" [IntegerLiteral 4, IntegerLiteral 3], IntegerLiteral 6], IntegerLiteral 2]
  print $ evalAST example
