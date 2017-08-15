{- My implementation of the adaboost algorithm, an algorithm that improves
   binary classifiers by combining many of them.
-}
module Adaboost where

import Data.List             (genericLength,foldl')


data WeightedPoint a = Wpt {weight :: Float,
                            label  :: Bool,
                            point  :: a } deriving (Show)


predictorError :: [WeightedPoint a] -> (a -> Bool) -> Float
{- Returns the 1/0 Error of a predictor -}
predictorError wpts predictor = sum $ map errors wpts
  where
    errors wpt = if label wpt /= (predictor $ point wpt) then weight wpt else 0


weighAndLabelPoints :: [a] -> [a] -> [WeightedPoint a]
{- Two lists of raw data into one list of weighted, labeled points -}
weighAndLabelPoints classA classB = wclassA ++ wclassB
  where
    wclassA = map (Wpt (0.5 / genericLength classA) True) classA
    wclassB = map (Wpt (0.5 / genericLength classB) False) classB


reWeightDistribution :: [WeightedPoint a] -> (a -> Bool) -> [WeightedPoint a]
{- Use Adaboost formula to increase weight of misclassified and decrease weight
 - of correctly classified points
 -}
reWeightDistribution wpts predictor = map reWeightPoint wpts
  where
    err = predictorError wpts predictor
    alpha = 0.5 * log((1 - err)/ err)
    normalizer = 2 * (err * (1-err)) ** 0.5
    reWeightPoint (Wpt {weight = w, label = lab, point = pt}) = Wpt w' lab pt
      where
        correct = lab == predictor pt
        w' = w / normalizer * if correct then exp alpha else exp (-alpha)


trainAdaboostedClassifier :: [WeightedPoint a]                -- Data
                          -> ([WeightedPoint a] -> a -> Bool) -- Classfier trainer
                          -> Float                            -- FPR threshold
                          -> (a -> Bool)                      -- Boosted classifier
{- Add weak classifiers until false positive rate is below fpr_threshold. -}
trainAdaboostedClassifier distribution trainer fpr_threshold = boosted
  where
    boosted = undefined


-- Cascade of Adaboosted Classifiers
type FalsePostitiveRate = Double

cascade :: [a] -> [a] -> [FalsePostitiveRate] -> (a -> Bool)
cascade faces backgrounds fprs = undefined
