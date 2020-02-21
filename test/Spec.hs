{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import DataTypes
import Parser

main :: IO ()
main = hspec $
  describe "Parsers" $ do
    describe "pVariable" $ do
      it "returns an Expression with Var constructor" $
        parse pVariable "" "x" `shouldParse` (Var "x")
      it "should not parse an empty string" $
        parse pVariable "" `shouldFailOn` ""
      it "should not parse an integer" $
        parse pVariable "" `shouldFailOn` "1"

    describe "pInteger" $ do
      it "returns an Expression with Const constructor" $
        parse pInteger "" "1" `shouldParse` (Const 1)
      it "should not parse an empty string" $
        parse pInteger "" `shouldFailOn` ""
      it "should not parse an variable" $
        parse pInteger "" `shouldFailOn` "x"

    describe "parens" $ do
      it "returns an Expression between parenthesis" $
        parse (parens pInteger) "" "(1)" `shouldParse` (Const 1)
      it "should not parse an empty string" $
        parse (parens pInteger) "" `shouldFailOn` ""
      it "should not parse anything without parenthesis" $
        parse (parens pInteger) "" `shouldFailOn` "1"

    describe "pExpression" $ do
      it "should ensure multiplication has precedence over addition" $
        parse pExpression "" "1 + 1 * 1" `shouldParse` (Sum (Const 1) (Product (Const 1) (Const 1)))
      it "should ensure functions have precedence over multiplication" $
        parse pExpression "" "g $ x * 1" `shouldParse` (Product (Func (Var "g") (Var "x")) (Const 1))
      it "should ensure exponentiation has precedence over functions" $
        parse pExpression "" "g $ x ^ 2" `shouldParse` (Func (Var "g") (Expt (Var "x") (Const 2)))
      it "should ensure unary operators have precedence over exponentiation" $
        parse pExpression "" "- x ^ 2" `shouldParse` (Expt (Negation (Var "x")) (Const 2))
      it "should reject malformed expressions" $
        parse pExpression "" `shouldFailOn` "^ 2 + 1"

    describe "pDeriv" $ do
      it "returns a Derivation" $
        parse pDeriv "" "deriv x x ^ 2 + 1" `shouldParse` (Derivation (Var "x") (Sum (Expt (Var "x") (Const 2)) (Const 1)))
      it "should reject misspelled deriv" $
        parse pDeriv "" `shouldFailOn` "derv x x ^ 2 + 1"
