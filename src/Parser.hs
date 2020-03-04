{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DataTypes
import Utils

type Parser = Parsec Void String

-- Add laws here
inputLaws :: [String]
inputLaws = [
  "Simplification Law 0: x + 0 = x"
  , "Simplification Law 1: 0 + x = x"
  , "Simplification Law 2: 0 * x = 0"
  , "Sum of derivatives: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
  ]
laws :: [Law]
laws = map (extract.(parse pLaw "")) inputLaws
-- sample expressions
expr1 :: Expression
expr1 = Deriv (Var "x") (Sum (Var "x") (Sum (Expt (Var "x") (Const 2)) (Const 7)))

-- Space consumer.
sc :: Parser ()
sc = L.space
  space1
  empty
  empty
-- Empty sc
sc' :: Parser ()
sc' = L.space
  empty
  empty
  empty

-- lexeme that doesn't consume space
lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- upto
upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

-- Parse Laws
pLaw :: Parser Law
pLaw = Law <$> lexeme (upto ':') <*> pExpression <* symbol "="  <*> pExpression

-- Parse variable.
pVariable :: Parser Expression
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- Parse function terms
pFunc :: Parser Expression
--pFunc = do name <- pVariable
--           expr <- parens pExpression
--           return (Func (getName name) expr)
pFunc = Func <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar)
  <*> (parens pExpression)

-- Parse deriv terms
pDeriv :: Parser Expression
{-pDeriv = do _ <- symbol "deriv("
            v <- pVariable
            _ <- symbol ","
            expr <- pExpression
            _ <- symbol ")"
            return (Deriv v expr)
            -}
pDeriv = pure Deriv <* symbol "deriv(" <*> pVariable <* symbol "," <*> pExpression <* symbol ")" 

pFuncVar :: Parser Expression
pFuncVar = try pDeriv <|> try pFunc <|> pVariable
  
-- Parse integers.
pInteger :: Parser Expression
pInteger = Const <$> lexeme L.decimal

-- Parse expressions between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Operator precedence where first in list is highest precedence.
operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ binary "^" Expt
    ]
  , [ prefix "-" Negation
    , prefix "+" id
    ] 
--  , [ binary "$" Func
--    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Sub
    ]
  ]

-- Binary operators.
binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

-- Prefix operators.
prefix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)

-- Parse terms.
pTerm :: Parser Expression
pTerm = choice
  [ parens pExpression
  , pFuncVar
--  , pVariable
  , pInteger
  ]

-- Expression parser.
pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable

-- Top level parser that is used to parse user input.
