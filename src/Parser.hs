{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DataTypes

type Parser = Parsec Void String

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

-- lexeme that consumes at least one space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Turn a String into a Parser String that selects for that String.
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
pFunc = Func <$> lexeme
  pVariable
  <*> (parens pExpression)

-- Parse deriv terms
pDeriv :: Parser Expression
pDeriv = pure Deriv <* symbol "deriv(" <*> pVariable <* symbol "," <*> pExpression <* symbol ")" 

-- Parse either a function or a variable.
-- Try in the order of derivative, function, variable since a variable
-- can match deriv or a function name and a function can match deriv.
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
  , pInteger
  ]

-- Expression parser.
pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable
