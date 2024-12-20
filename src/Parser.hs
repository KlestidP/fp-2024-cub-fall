module Parser (
    Expr(..),
    parseExpr,
    parseNumber
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity (Identity)

-- Expression types
data Expr 
    = Num Double 
    | Var String 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Mul Expr Expr 
    | Div Expr Expr 
    | Pow Expr Expr
    | Sqrt Expr
    | Neg Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | Log Expr
    | Ln Expr
    | Pi
    | E
    deriving (Show)

-- Lexer definition
lexer :: TokenParser ()
lexer = makeTokenParser emptyDef {
    reservedNames = ["sin", "cos", "tan", "log", "ln", "pi", "e", "sqrt"]
}

-- Parsing functions
parseExpr :: Parser Expr
parseExpr = spaces *> buildExpressionParser operators parseFactor <* spaces <?> "expression"

operators :: OperatorTable String () Identity Expr
operators = [
    [Prefix (reservedOp lexer "-" >> return Neg)],
    [Infix (reservedOp lexer "^" >> return Pow) AssocRight],
    [Infix (reservedOp lexer "*" >> return Mul) AssocLeft,
     Infix (reservedOp lexer "/" >> return Div) AssocLeft],
    [Infix (reservedOp lexer "+" >> return Add) AssocLeft,
     Infix (reservedOp lexer "-" >> return Sub) AssocLeft]
  ]

-- Parsing trig and log functions
parseFunc :: Parser Expr
parseFunc = do
    func <- choice $ map (try . string) ["sin", "cos", "tan", "log", "ln", "sqrt"]
    spaces
    expr <- parseFactor
    return $ case func of
        "sin" -> Sin expr
        "cos" -> Cos expr
        "tan" -> Tan expr
        "log" -> Log expr
        "sqrt" -> Sqrt expr
        "ln" -> Ln expr
        _ -> error ("Unknown function: " ++ func)

parseConstant :: Parser Expr
parseConstant = (string "pi" >> return Pi) <|> (string "e" >> return E)

parseNumber :: Parser Expr
parseNumber = do
    whole <- many1 digit
    fractional <- optionMaybe $ do
        char '.'
        many1 digit
    let numStr = case fractional of
                    Just frac -> whole ++ "." ++ frac
                    Nothing   -> whole
    return $ Num (read numStr)

parseFactor :: Parser Expr
parseFactor = spaces *> (parseConstant
           <|> parseFunc
           <|> parseVariable 
           <|> parseNumber
           <|> parens lexer parseExpr) <* spaces

parseVariable :: Parser Expr
parseVariable = Var <$> many1 letter <?> "variable"
