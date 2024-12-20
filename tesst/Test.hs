{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Data.Map as Map
import Text.Parsec (parse)
import Test.HUnit
import Test.QuickCheck
import Parser
import Evaluator (evaluate, EvalResult, ErrorType(..))
import Memory

-- function to compare floating-point values within a margin of error
assertApproxEqual :: String -> Double -> EvalResult -> Assertion
assertApproxEqual desc expected (Right actual) =
    let epsilon = 1e-10  -- Allowable error margin
    in assertBool (desc ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")") 
       (abs (expected - actual) < epsilon)
assertApproxEqual desc _ (Left err) =
    assertFailure $ desc ++ " (evaluation failed with error: " ++ show err ++ ")"

-- unit tests for parsing
parseNumTest :: Test
parseNumTest = TestCase $ do
    let result = parse parseExpr "" "42"
    case result of
        Right (Num x) -> assertEqual "Number parsing" 42.0 x
        _ -> assertFailure $ "Failed to parse number: " ++ show result

parseBinaryOpTest :: Test
parseBinaryOpTest = TestCase $ do
    let result = parse parseExpr "" "3 + 4"
    case result of
        Right (Add (Num 3) (Num 4)) -> return ()
        _ -> assertFailure $ "Failed to parse binary operation: " ++ show result

-- unit tests for function evaluation
evalSinTests :: Test
evalSinTests = TestList [
    TestCase $ assertApproxEqual "Sin(0)" 0.0 (fst $ evaluate (Sin (Num 0)) initialState),
    TestCase $ assertApproxEqual "Sin(pi/2)" 1.0 (fst $ evaluate (Sin (Div Pi (Num 2))) initialState),
    TestCase $ assertApproxEqual "Sin(pi)" 0.0 (fst $ evaluate (Sin Pi) initialState)
    ]

evalCosTests :: Test
evalCosTests = TestList [
    TestCase $ assertApproxEqual "Cos(0)" 1.0 (fst $ evaluate (Cos (Num 0)) initialState),
    TestCase $ assertApproxEqual "Cos(pi/2)" 0.0 (fst $ evaluate (Cos (Div Pi (Num 2))) initialState),
    TestCase $ assertApproxEqual "Cos(pi)" (-1.0) (fst $ evaluate (Cos Pi) initialState)
    ]

evalLogTests :: Test
evalLogTests = TestList [
    TestCase $ assertApproxEqual "Log(100)" 2.0 (fst $ evaluate (Log (Num 100)) initialState),
    TestCase $ assertApproxEqual "Log(10)" 1.0 (fst $ evaluate (Log (Num 10)) initialState),
    TestCase $ assertApproxEqual "Log(1)" 0.0 (fst $ evaluate (Log (Num 1)) initialState)
    ]

evalLnTests :: Test
evalLnTests = TestList [
    TestCase $ assertApproxEqual "Ln(1)" 0.0 (fst $ evaluate (Ln (Num 1)) initialState),
    TestCase $ assertApproxEqual "Ln(e)" 1.0 (fst $ evaluate (Ln E) initialState),
    TestCase $ assertApproxEqual "Ln(Exp(2))" 2.0 (fst $ evaluate (Ln (Num (exp 2))) initialState)
    ]

evalPiTests :: Test
evalPiTests = TestList [
    TestCase $ assertApproxEqual "Pi" pi (fst $ evaluate Pi initialState),
    TestCase $ assertApproxEqual "Pi / 2" (pi / 2) (fst $ evaluate (Div Pi (Num 2)) initialState),
    TestCase $ assertApproxEqual "2 * Pi" (2 * pi) (fst $ evaluate (Mul (Num 2) Pi) initialState)
    ]

evalETests :: Test
evalETests = TestList [
    TestCase $ assertApproxEqual "E" (exp 1) (fst $ evaluate E initialState),
    TestCase $ assertApproxEqual "E^2" 7.38905609893065 (fst $ evaluate (Pow E (Num 2)) initialState),
    TestCase $ assertApproxEqual "E^0" 1.0 (fst $ evaluate (Pow E (Num 0)) initialState)
    ]

-- ^quickCheck property tests
prop_addCommutative :: Double -> Double -> Bool
prop_addCommutative x y =
    fst (evaluate (Add (Num x) (Num y)) initialState) ==
    fst (evaluate (Add (Num y) (Num x)) initialState)

prop_mulCommutative :: Double -> Double -> Bool
prop_mulCommutative x y =
    fst (evaluate (Mul (Num x) (Num y)) initialState) ==
    fst (evaluate (Mul (Num y) (Num x)) initialState)

prop_addCommutativeComplex :: Expr -> Expr -> Bool
prop_addCommutativeComplex e1 e2 =
    fst (evaluate (Add e1 e2) initialState) ==
    fst (evaluate (Add e2 e1) initialState)

prop_mulCommutativeComplex :: Expr -> Expr -> Bool
prop_mulCommutativeComplex e1 e2 =
    fst (evaluate (Mul e1 e2) initialState) ==
    fst (evaluate (Mul e2 e1) initialState)

instance Arbitrary Expr where
    arbitrary = do
        let base = oneof [Num <$> arbitrary]
        sized $ \n ->
            if n == 0
            then base
            else oneof
                [ Add <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , Mul <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , base
                ]

prop_memoryStorage :: String -> Double -> Bool
prop_memoryStorage key value =
    let state = storeMemory key value initialState
    in case Map.lookup key (memory state) of
        Just v -> v == value
        Nothing -> False

main :: IO ()
main = do
    putStrLn "Running HUnit tests..."
    _ <- runTestTT (TestList [
        parseNumTest,
        parseBinaryOpTest,
        evalSinTests,
        evalCosTests,
        evalLogTests,
        evalLnTests,
        evalPiTests,
        evalETests
        ])
    putStrLn "Running QuickCheck tests..."
    quickCheck prop_addCommutative
    quickCheck prop_mulCommutativeComplex
    quickCheck prop_addCommutativeComplex
    quickCheck prop_mulCommutative
    quickCheck prop_memoryStorage
