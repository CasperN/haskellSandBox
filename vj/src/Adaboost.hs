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
    errors wpt = if label wpt /= predictor (point wpt) then weight wpt else 0


weighAndLabelPoints :: [a] -> [a] -> [WeightedPoint a]
{- Two lists of raw data into one list of weighted, labeled points -}
weighAndLabelPoints classA classB = wclassA ++ wclassB
  where
    wclassA = map (Wpt (0.5 / genericLength classA) True) classA
    wclassB = map (Wpt (0.5 / genericLength classB) False) classB


reWeightDistribution :: [WeightedPoint a]          -- Old distribution
                     -> (a -> Bool)                -- Trained decision stump
                     -> ([WeightedPoint a], Float) -- New distribution, Alpha
{- Use Adaboost formula to increase weight of misclassified and decrease weight
 - of correctly classified points
 -}
reWeightDistribution wpts predictor = (map reWeightPoint wpts, alpha)
  where
    err = predictorError wpts predictor
    alpha = 0.5 * log((1 - err)/ err)
    normalizer = 2 * sqrt(err * (1-err))
    reWeightPoint Wpt {weight = w, label = lab, point = pt} = Wpt w' lab pt
      where
        correct = lab == predictor pt
        w' = w / normalizer * if correct then exp alpha else exp (-alpha)


falsePositiveRate :: [WeightedPoint a] -> (a -> Bool) -> Float
{- Sum of weights where label is false but the point was classifed as true -}
falsePositiveRate distribution classifier = fpr
  where
    falsePositives wp = not (label wp) && classifier (point wp)
    fpr = sum.map weight $ filter falsePositives distribution



trainAdaboostedClassifier :: [WeightedPoint a]                -- Data
                          -> ([WeightedPoint a] -> a -> Bool) -- Classfier trainer
                          -> Float                            -- FPR threshold
                          -> (a -> Bool)                      -- Boosted classifier
{- Add weak classifiers until false positive rate is below fpr_threshold.
 - Continues training until there exists a threshold for the sum of alpha-weighted
 - scorers to correctly score all faces with a false positive rate below
 - that specified.
 - However it uses the 0 threshold in the returned boosted weighed clasifier
 -}
trainAdaboostedClassifier distribution trainer fpr_threshold = boosted classifiers
  where
    -- Given distribution train next classifier and reweigh the distribution
    nextDistAndWeighedClassifier dist = (dist', weighedClassifier)
      where
        -- Weigh classifier vote as per Adaboost
        classifier = trainer dist
        (dist', alpha) = reWeightDistribution dist classifier
        weighedClassifier ii = if classifier ii then alpha else - alpha

    -- Train first weighed classifier
    (dist1, wc0) = nextDistAndWeighedClassifier distribution
    -- Train more classifiers until false positive rate is below threshold
    (_, classifiers) = until belowFprTh nextWeighedClassifier (dist1, [wc0])

    -- Next classifier updates distribution and appends to classifiers
    nextWeighedClassifier (dist, wClassifiers) = (dist', wClassifiers')
      where
        (dist', nextWeighedClassifier) = nextDistAndWeighedClassifier dist
        wClassifiers' = nextWeighedClassifier : wClassifiers
    -- Check the fpr of sum of the combined (boosted) classifiers
    belowFprTh (dist,cs) = fpr_threshold > falsePositiveRate dist (boosted cs)
    -- Turns a list of weighed classifiers into a single binary classifier
    boosted classifiers ii = 0 < sum( map ($ii) classifiers)


-- Cascade of Adaboosted Classifiers
cascade :: [WeightedPoint a] -> [Float] -> ([WeightedPoint a] -> a -> Bool)
{- Train many adaboosted classifiers until False Positive Rate < threhsolds
 - Filter out easy to classify Backgrounds in each stage of cascade
 -}
cascade wpts fpr_thresholds trainer = undefined
