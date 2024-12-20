module Evaluator (
    evaluate,
    ErrorType(..),
    EvalResult
) where


import Parser
import Memory
import Control.Monad.State
import qualified Data.Map as Map

-- define error types
data ErrorType
    = DivisionByZero
    | NegativeSqrt
    | VariableNotFound String
    deriving (Show, Eq)

type EvalResult = Either ErrorType Double

-- evaluate an expression
evaluate :: Expr -> CalcState -> (EvalResult, CalcState)
evaluate expr calcState = runState (evalExpr expr) calcState

-- evaluation function
evalExpr :: Expr -> State CalcState EvalResult
evalExpr (Num x) = return $ Right x
evalExpr (Var name) = do
    st <- get
    case Map.lookup name (memory st) of
        Just val -> return $ Right val
        Nothing -> return $ Left (VariableNotFound name)
evalExpr (Add x y) = liftA2 (+) <$> evalExpr x <*> evalExpr y
evalExpr (Sub x y) = liftA2 (-) <$> evalExpr x <*> evalExpr y
evalExpr (Mul x y) = liftA2 (*) <$> evalExpr x <*> evalExpr y
evalExpr (Div x y) = do
    divisor <- evalExpr y
    case divisor of
        Right 0 -> return $ Left DivisionByZero
        Right val -> liftA2 (/) <$> evalExpr x <*> pure (Right val)
        Left err -> return $ Left err
evalExpr (Pow x y) = liftA2 (**) <$> evalExpr x <*> evalExpr y
evalExpr (Sqrt x) = do
    value <- evalExpr x
    case value of
        Right v | v < 0 -> return $ Left NegativeSqrt
        Right v -> return $ Right (sqrt v)
        Left err -> return $ Left err
evalExpr (Neg x) = fmap negate <$> evalExpr x
evalExpr (Sin x) = fmap sin <$> evalExpr x
evalExpr (Cos x) = fmap cos <$> evalExpr x
evalExpr (Tan x) = fmap tan <$> evalExpr x
evalExpr (Log x) = fmap (logBase 10) <$> evalExpr x
evalExpr (Ln x) = fmap log <$> evalExpr x
evalExpr Pi = return $ Right pi
evalExpr E = return $ Right (exp 1)
