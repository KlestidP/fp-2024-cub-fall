module Main (main) where

import Parser (Expr(..), parseExpr, parseNumber)
import Evaluator
import Memory
import Text.Parsec (parse, ParseError, (<|>), try, string, spaces, char, many1, letter)
import Text.Parsec.String (Parser)
import qualified Data.Map as Map
import Text.Printf (printf)  -- For clean floating-point formatting

-- command type to handle different user inputs
data Command
    = ExprCommand Expr        -- mathematical expression
    | MemorySet String Double -- set a memory value
    | MemoryPrint             -- print memory
    deriving (Show)

-- parse commands
parseCommand :: String -> Either ParseError Command
parseCommand input = parse commandParser "" input

commandParser :: Parser Command
commandParser = try parseMemorySet <|> try parseMemoryPrint <|> parseExprCommand

parseMemorySet :: Parser Command
parseMemorySet = do
    string "memory"
    spaces
    key <- many1 letter
    spaces
    char '='
    spaces
    expr <- parseExpr
    return $ MemorySet key (evaluateExpr expr)
  where
    evaluateExpr :: Expr -> Double
    evaluateExpr (Num x) = x
    evaluateExpr _ = error "Invalid memory value: expected a constant"

parseMemoryPrint :: Parser Command
parseMemoryPrint = string "memory" >> return MemoryPrint

parseExprCommand :: Parser Command
parseExprCommand = ExprCommand <$> parseExpr

repl :: CalcState -> IO ()
repl state = do
    putStr "> "
    input <- getLine
    case parseCommand input of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            repl state
        Right (ExprCommand expr) -> do
            let (result, newState) = evaluate expr state
            case result of
                Left DivisionByZero -> putStrLn "Error: Division by zero."
                Left NegativeSqrt   -> putStrLn "Error: Negative value for square root."
                Left (VariableNotFound var) ->
                    putStrLn $ "Error: Variable '" ++ var ++ "' not found."
                Right value -> putStrLn $ "Result: " ++ printf "%.10g" value
            repl newState
        Right MemoryPrint -> do
            if null (Map.toList $ memory state)
                then putStrLn "Memory is empty."
                else do
                    putStrLn "Memory:"
                    mapM_ (\(key, value) -> putStrLn $ key ++ " = " ++ printf "%.10g" value) (Map.toList $ memory state)
            repl state
        Right (MemorySet key value) -> do
            let newState = storeMemory key value state
            putStrLn $ "Stored: " ++ key ++ " = " ++ printf "%.10g" value
            repl newState


main :: IO ()
main = do
    putStrLn "Scientific Calculator REPL"
    repl initialState
