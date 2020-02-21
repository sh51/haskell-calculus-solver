module Solver where

import DataTypes

sampleLaws :: Law
sampleLaws = Law "Commutativity of addiction" (Sum (Const 3) (Const 4)) (Sum (Const 4) (Const 3))

sampleLawName :: String
sampleLawName = "Commutativity of addiction"
