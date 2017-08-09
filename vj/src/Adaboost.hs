{- Short Desc
   Long Desc
-}
module Adaboost where

import Data.List             (genericLength)


predictorError :: (Num n) => [WeightedPoint a] ->
                            (a -> bool) ->
                            n
{- Returns the Error of a predictor-}
predictorError = undefined





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
reWeightDistribution :: [WeightedPoint a] -> (a -> Bool) -> [WeightedPoint a]
reWeightDistribution wps stump = undefined

trainAdaboostedClassifier :: [WeightedPoint a] ->              -- Data
                             (WeightedPoint a -> a -> Bool) -> -- Classfier trainer
                             Float ->                          -- FPR threshold
                             (a -> Bool)                       -- Boosted
trainAdaboostedClassifier distribution trainer fpr_threshold = undefined


-- Cascade of Adaboosted Classifiers
type FalsePostitiveRate = Double

cascade :: [a] -> [a] -> [FalsePostitiveRate] -> (a -> Bool)
cascade faces backgrounds fprs = undefined
