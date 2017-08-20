{- My implementation of the adaboost algorithm, an algorithm that improves
 - binary classifiers by combining many of them.
 -
 - We make a cascade of adaboosted classifers by training over all data then
 - filtering out the negatives which are easily classifed as such.
 -
 -}
module Adaboost where

import Data.List             (genericLength,foldl')
import Debug.Trace

data WeightedPoint a = Wpt {weight :: Float, label :: Bool, point :: a}
  deriving (Show)


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


splitOnLabel :: [WeightedPoint a] -> ([WeightedPoint a],[WeightedPoint a])
{- Returns the points that are positive and those that are negative labeled -}
splitOnLabel dist = foldl' separate ([],[]) dist
  where
    separate (pos, neg) x = if label x then (x:pos, neg) else (pos, x:neg)


reWeightDistribution :: [WeightedPoint a]          -- Old distribution
                     -> (a -> Bool)                -- Trained decision stump
                     -> ([WeightedPoint a], Float) -- New distribution, Alpha
{-
 - Use Adaboost formula to increase weight of misclassified and decrease weight
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
        w' = w / normalizer * if correct then exp(-alpha) else exp alpha


boostingDist :: ([WeightedPoint a] -> a -> Bool)   -- Learner
             -> [WeightedPoint a]                  -- Data
             -> [([WeightedPoint a],[a -> Float])] -- List of trained
{-
 - Add weak classifiers and reweights distribution forever. Each element is a
 - weighted distribution and list of weighted binary classifiers. The sign of
 - the sum of those classifiers is itself a binary classifier.
 -}
boostingDist learnToClassify dist0 = iterate nextDistribution (dist1,[wc0])
  where
    -- Weigh classifier and update distribution vote as per Adaboost
    trainClassifier dist = (dist', weighedClassifier)
      where
        classifier = learnToClassify dist
        (dist', alpha) = reWeightDistribution dist classifier
        weighedClassifier ii = if classifier ii then alpha else - alpha
    -- Train first weighed classifier
    (dist1, wc0) = trainClassifier dist0
    -- Update distribution and append to list of classifiers
    nextDistribution (dist, wClassifiers) = (dist', wClassifiers')
      where
        (dist', nextWeighedClassifier) = trainClassifier dist
        wClassifiers' = nextWeighedClassifier : wClassifiers

weightedVote :: [(a -> Float)] -> a -> Float
{- Combines the weighed classifiers -}
weightedVote classifiers ii = sum( map ($ii) classifiers)


violaJonesTrain :: [([WeightedPoint a],[a->Float])] -- Distributions, classifers
                -> Float                       -- FPR threhsold
                -> [a -> Float]                -- Qualified weighted classifiers
{-
 - Trains classifiers until false positive rate is below a given threshold while
 - the false negative rate is artifically set to zero.
 -}
violaJonesTrain boosting fprThreshold =  snd. head $ dropWhile aboveThr boosting
  where
    aboveThr (dist, classifiers) = fpr > fprThreshold
      where
        score = weightedVote classifiers
        (positives, negatives) = splitOnLabel dist
        -- Threshold where false negatives rate = 0
        th = minimum $ map (score . point) positives
        -- False positive rate at this threshold
        bs = map (\wp -> (weight wp, score $ point wp)) $ negatives
        falsePositives = sum $ map fst $ filter (\x -> th < snd x) bs
        allNegatives = sum $ map fst bs
        fpr = falsePositives / allNegatives


filterEasyNegatives :: (a -> Float)      -- Scorer
                    -> [WeightedPoint a] -- Old distribution
                    -> [WeightedPoint a] -- New distribution
{-
 - Removes all -labeled where score below the lowest score of the +labeleds,
 - then reweights the distribuion so half the mass is in each category.
 -}
filterEasyNegatives scorer dist = (t1.t2) $ newDist
  where
    (positives, negatives) = splitOnLabel dist
    th = minimum $ map (scorer.point) positives
    negatives' = filter (\x -> th < scorer(point x)) negatives
    newDist = weighAndLabelPoints (map point positives) (map point negatives')
    -- DEBUG
    t1 = trace ("# Pos: " ++ show(length positives))
    t2 = trace ("# Neg: " ++ show(length negatives'))


-- Cascade of Adaboosted Classifiers
trainCascade :: [WeightedPoint a]                -- Distribution
             -> [Float]                          -- FPR thresholds
             -> ([WeightedPoint a] -> a -> Bool) -- Train classifer
             -> [a -> Bool]                      -- Classifier cascade
{-
 - Train many adaboosted classifiers until False Positive Rate < threhsolds
 - Filter out easy to classify Backgrounds in each stage of cascade
 -}
trainCascade wpts fprThresholds trainer = cascade
  where
    cascade = snd $ foldl trainToThreshold (wpts, []) fprThresholds
    -- Each classifer in cascade is adaboosted
    trainToThreshold (dist, classfiers) fprT = logger $ (dist', c : classfiers)
      where
        logger = trace ("Error: " ++
              show(predictorError dist (classifyWithCascade classfiers)))

        -- Train until adjusted false positive rate < threhsold
        scorers = violaJonesTrain (boostingDist trainer dist) fprT
        wScr = weightedVote scorers
        -- next classifier in cascade
        c = \x -> (0 < wScr x)
        -- Filter easy to cassify negatives
        dist' = filterEasyNegatives wScr dist


classifyWithCascade :: [a -> Bool] -> a -> Bool
{- The final classifier -}
classifyWithCascade cascade x = and $ map ($x) cascade
