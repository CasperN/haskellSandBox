{- Short Desc
   Long Desc
-}
module Adaboost where

import Data.List             (genericLength)


data WeightedPoint a = Wpt {weight :: Float,
                            label  :: Bool,
                            point  :: a}
                          deriving (Show)



weighImages :: [a] -> [a] -> [WeightedPoint a]
{- labels if they're faces or backgrounds and weighs the data set such that
half the mass is on faces while the other half is on backgrounds -}
weighImages faces backgrounds = wfaces ++ wbacks
  where
    wfaces = map (Wpt wf True) faces
    wbacks = map (Wpt wb False) backgrounds
    wf = 0.5 / genericLength faces
    wb = 0.5 / genericLength backgrounds


-- Adaboost
reWeightDistribution :: [WeightedPoint a] -> (a -> Double) -> [WeightedPoint a]
reWeightDistribution iis stump = undefined

trainAdaboostedClassifier :: [WeightedPoint a] -> (a -> Double)
trainAdaboostedClassifier distribution = undefined


-- Cascade of Adaboosted Classifiers
type FalsePostitiveRate = Double

cascade :: [a] -> [a] -> [FalsePostitiveRate] -> (a -> Bool)
cascade faces backgrounds fprs = undefined
