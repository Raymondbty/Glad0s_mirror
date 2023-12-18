-- Definition of the abstract syntax tree (AST) data type
data Ast =
  Define { variable :: String, expression :: Ast }  -- Define a variable
  | IntegerLiteral Int                             -- Integer literal
  | Symbol String                                  -- Symbolic expression
  | BooleanLiteral Bool                            -- Boolean literal
  | Call { functionName :: String, arguments :: [Ast] }  -- Function call
  deriving (Show, Eq)

-- Function to evaluate an AST and return a Maybe result
evalAST :: Ast -> Maybe Ast
-- Evaluation for IntegerLiteral returns the same integer value wrapped in Just
evalAST (IntegerLiteral n) = Just $ IntegerLiteral n
-- Evaluation for Symbol always returns Nothing (undefined)
evalAST (Symbol _) = Nothing
-- Evaluation for BooleanLiteral returns the same boolean value wrapped in Just
evalAST (BooleanLiteral b) = Just $ BooleanLiteral b
-- Evaluation for Define always returns Nothing (undefined)
evalAST (Define _ _) = Nothing

-- Evaluation for addition operation
evalAST (Call "+" args) = do
  -- Recursively evaluate each argument
  evaluatedArgs <- mapM evalAST args
  -- Sum the integer values of the evaluated arguments
  let result = sum [n | IntegerLiteral n <- evaluatedArgs]
  -- Wrap the result in Just
  Just $ IntegerLiteral result

-- Evaluation for multiplication operation
evalAST (Call "*" args) = do
  -- Recursively evaluate each argument
  evaluatedArgs <- mapM evalAST args
  -- Multiply the integer values of the evaluated arguments
  let result = product [n | IntegerLiteral n <- evaluatedArgs]
  -- Wrap the result in Just
  Just $ IntegerLiteral result

-- Evaluation for division operation
evalAST (Call "/" args) = do
  -- Recursively evaluate each argument
  evaluatedArgs <- mapM evalAST args
  -- Perform division if there are two integer arguments
  let result = case evaluatedArgs of
                 [IntegerLiteral n1, IntegerLiteral n2] ->
                   if n2 /= 0 then n1 `div` n2 else error "Division by zero"
                 _ -> error "Invalid arguments for division"
  -- Wrap the result in Just
  Just $ IntegerLiteral result

-- Evaluation for subtraction operation
evalAST (Call "-" [arg1, arg2]) = do
  -- Recursively evaluate each argument
  evalArg1 <- evalAST arg1
  evalArg2 <- evalAST arg2
  -- Subtract two integer values if both arguments are integers
  case (evalArg1, evalArg2) of
    (IntegerLiteral n1, IntegerLiteral n2) -> Just $ IntegerLiteral (n1 - n2)
    _ -> Nothing

-- Evaluation for 'if' condition
evalAST (Call "if" [condition, trueBranch, falseBranch]) = do
  -- Evaluate the condition
  evalCondition <- evalAST condition
  -- Choose the branch based on the boolean value of the condition
  case evalCondition of
    BooleanLiteral True -> evalAST trueBranch
    BooleanLiteral False -> evalAST falseBranch
    _ -> Nothing

-- Default case for unrecognized function calls
evalAST (Call _ _) = Nothing

-- Main function to demonstrate the evaluation of an example expression
main :: IO ()
main = do
  -- Example expression: (/ (* (+ 4 3) 6) 2)
  let example = Call "/" [Call "*" [Call "+" [IntegerLiteral 4, IntegerLiteral 3], IntegerLiteral 6], IntegerLiteral 2]
  -- Print the result of evaluating the example expression
  print $ evalAST example
