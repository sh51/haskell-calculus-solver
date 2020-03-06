{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import DataTypes
import Parser
import Utils

main :: IO ()
main = hspec $
  describe "haskell-calculus-solver" $ do
    describe "Parsers" $ do
      describe "pVariable" $ do
        it "returns an Expression with Var constructor" $
          parse pVariable "" "x" `shouldParse` (Var "x")
        it "should not parse an empty string" $
          parse pVariable "" `shouldFailOn` ""
        it "should not parse an integer" $
          parse pVariable "" `shouldFailOn` "1"

      describe "pFunc" $ do
        it "returns an Expression with Func constructor" $
          parse pFunc "" "g(x)" `shouldParse` (Func (Var "g") (Var "x"))

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
        it "should ensure exponentiation has precedence over negation" $
          parse pExpression "" "- x ^ 2" `shouldParse` (Negation (Expt (Var "x") (Const 2)))
        it "should reject malformed expressions" $
          parse pExpression "" `shouldFailOn` "^ 2 + 1"

      describe "pDeriv" $ do
        it "returns a Derivation" $
          parse pDeriv "" "deriv(x, x ^ 2 + 1)" `shouldParse` (Deriv (Var "x") (Sum (Expt (Var "x") (Const 2)) (Const 1)))
        it "should reject misspelled deriv" $
          parse pDeriv "" `shouldFailOn` "derv(x, x ^ 2 + 1)"

    describe "Utils" $ do
      describe "match" $ do
        it "should match a variable to any expression" $
          match (Var "x") (Const 0) `shouldBe` [("x", Const 0)]
        it "should match equal constants" $
          match (Const 0) (Const 0) `shouldBe` [("0", Const 0)]
        it "should not match unequal constants" $
          match (Const 0) (Const 1) `shouldBe` []
        it "should match binary operators" $
          match (Sum (Var "x") (Const 0)) (Sum (Const 0) (Const 0))
          `shouldBe`
          [("x", Const 0), ("0", Const 0)]
        it "should match functions of the same name" $
          match (Func (Var "f") (Var "x")) (Func (Var "f") (Var "a"))
          `shouldBe`
          [("x", Var "a")]
        it "should not match functions of different names" $
          match (Func (Var "f") (Var "x")) (Func (Var "g") (Var "a"))
          `shouldBe`
          []

      describe "apply" $ do
        it "should substitute matching variables" $
          apply ("x", Var "y") (Var "x") `shouldBe` (Var "y")
        it "should not substitute variables that don't match" $
          apply ("x", Var "y") (Var "z") `shouldBe` (Var "z")
        it "should substitute for binary operations" $
          apply ("x", Var "y") (Sum (Var "x") (Const 0))
          `shouldBe`
          (Sum (Var "y") (Const 0))
        it "should substitute function arguments" $
          apply ("x", Var "a") (Func (Var "f") (Var "x"))
          `shouldBe`
          (Func (Var "f") (Var "a"))
        it "should substitute derivative equations" $
          apply ("x", Var "a") (Deriv (Var "x") (Var "x"))
          `shouldBe`
          (Deriv (Var "x") (Var "a"))

      describe "compatible" $ do
        it "should return True for compatible substitutions" $
          compatible ("x", Var "a") ("x", Var "a") `shouldBe` True
        it "should return False for incompatible substitutions" $
          compatible ("x", Var "a") ("x", Var "b") `shouldBe` False
        it "should return True for unrelated substitutions" $
          compatible ("x", Var "a") ("y", Var "b") `shouldBe` True

      describe "patMTop" $ do
        it "should return a substituted expression if the law applies" $
          patMTop (Law "" (Sum (Var "x") (Const 0)) (Var "x")) (Sum (Const 1) (Const 0))
          `shouldBe`
          [(Const 1)]
        it "should not return substituted expressions if the law doesn't apply" $
          patMTop (Law "" (Sum (Var "x") (Const 0)) (Var "x")) (Const 0)
          `shouldBe`
          []

      describe "rws" $ do
        it "should return the steps if they apply" $
          rws [(Law "Identity" (Sum (Var "x") (Const 0)) (Var "x"))] (Sum (Const 1) (Const 0))
          `shouldBe`
          [Step "Identity" (Const 1)]
        it "should not return any steps if they don't apply" $
          rws [(Law "Identity" (Sum (Var "x") (Const 0)) (Var "x"))] (Const 1)
          `shouldBe`
          []

      describe "generateLaws" $ do
        it "should return a list of laws" $
          generateLaws ["Identity: x + 0 = x"]
          `shouldBe`
          [Law "Identity" (Sum (Var "x") (Const 0)) (Var "x")]
