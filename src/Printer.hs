{-# OPTIONS_GHC -Wno-orphans #-}
module Printer where

import Data.Text.Prettyprint.Doc

import DataTypes

prettyCalculation :: Calculation -> Doc ann
-- prettyCalculation c@(Calculation (Deriv x e) steps)
--   = pretty c
prettyCalculation other = pretty other

instance Pretty Calculation where
  pretty (Calculation d steps) =  pretty d <> line <> cat (map pretty steps) <> line

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
  pretty (Prime e) = pretty e <> pretty "'"
  pretty (Deriv d e) = pretty ("d/d" ++ (getName d)) <> space <> pretty e

instance Pretty Step where
  pretty (Step s e) = indent 2 (pretty "{" <> pretty s <> pretty "}") <> line <> pretty e
