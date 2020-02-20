{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr
import Data.Char
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parse variable.
pVariable :: Parser Expression
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- Parse integers.
pInteger :: Parser Expression
pInteger = Const <$> lexeme L.decimal

-- Parse expressions between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse terms.
pTerm :: Parser Expression
pTerm = choice
  [ parens pExpression
  , pVariable
  , pInteger
  ]

-- Operator precedence where first in list is highest precedence.
operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "^" Expt
    ]
  , [ binary "$" Func
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

-- Expression parser.
pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable

-- > deriv(x, x^2)
-- What variable do you want to derive on? x
-- Equation to derive: x^2
-- "deriv("++var++", "++equation++")"

-- f $ a + 2 -> f(a) + 2
-- f $ a ^ 2 -> f(a) ^ 2 || f(a ^ 2)
-- f $ a * 3 -> f(a) * 3 || f(a * 2)
