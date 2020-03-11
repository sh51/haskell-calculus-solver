{-# OPTIONS_GHC -Wno-orphans #-}
module Printer where

import Data.Text.Prettyprint.Doc

import DataTypes

-- Wrapper function to prettify a Calculation.
prettyCalculation :: Calculation Expression -> Doc ann
prettyCalculation c = pretty c

instance Pretty a =>  Pretty (Calculation a) where
  pretty (Calculation d steps) =  pretty d <> line <> vsep (map pretty steps) <> line

instance Pretty Expression where
  pretty (Var x) = pretty x
  pretty (Const a) = pretty a
  pretty (Negation e) = pretty "-" <> pretty e
  pretty (Sum a b) = lparen <> pretty a <> pretty " + " <> pretty b <> rparen
  pretty (Sub a b) = lparen <> pretty a <> pretty " - " <> pretty b <> rparen
  pretty (Product a b) = lparen <> pretty a <> pretty " * " <> pretty b <> rparen
  pretty (Division a b) = lparen <> pretty a <> pretty " / " <> pretty b <> rparen
  pretty (Expt a b) = lparen <> pretty a <> pretty "^" <> pretty b <> rparen
  pretty (Func f e) = pretty f <> lparen <> pretty e <> rparen
  pretty (Deriv d e) = pretty ("d/d" ++ (getName d)) <> space <> pretty e

instance Pretty a => Pretty (Step a) where
  pretty (Step s e) = pretty "=" <> indent 1 (pretty "{" <> pretty s <> pretty "}") <> line <> pretty e
