{- Short Desc
   Long Desc
-}
module HaarFilter where

import Adaboost              (WeightedPoint, weight, label, point)
import Data.Array.Repa       hiding (map, (++))
import Data.List             (sortOn, groupBy, maximumBy)

type IntegralImage = Array U DIM2 Int


data FilterShape  = S1x1 |  S1x2 | S2x1 | S2x2 | S3x1 | S1x3 deriving (Show, Eq)
data FilterWindow = Window Int Int Int Int deriving (Show)
data FilterParams = Filter {shape :: FilterShape, window :: FilterWindow}
  deriving (Show)


{-# ANN calculateFeature "HLint: ignore Redundant bracket" #-}
calculateFeature :: FilterParams -> IntegralImage -> Int
{- Image subtracts different sections of a detector window (Haar Feature) -}
calculateFeature (Filter {shape = sh , window = Window i0 j0 i1 j1}) ii =
  case sh of
    -- Whole window
    S1x1 -> (rect i0 j0 i1 j1)
    -- Right - Left
    S1x2 -> (rect i0 j0 i1 j') - (rect i0 j' i1 j1)
      where
        j' = (j0 + j1) `div` 2
    -- Top - Bottom
    S2x1 -> (rect i0 j0 i' j1) - (rect i0 j1 i' j1)
      where
        i' = (i0 + i1) `div` 2
    -- Top Right + Bottom Left - Top Left - Bottom Right
    S2x2 -> (rect i' j' i1 j1) + (rect i0 j0 i' j') -
            (rect i' j0 i1 j') - (rect i0 j' i' j1)
      where
        i' = (i0 + i1) `div` 2
        j' = (j0 + j1) `div` 2
    -- Top - 2 * Middle + Bottom
    S3x1 -> (rect i0 j0 i'  j1) - 2 * (rect i'  j0 i'' j1) + (rect i'' j0 i1 j1)
      where
        deltaI = (i1 - i0) `div` 3
        i'  = i0 + deltaI
        i'' = i0 + deltaI + deltaI
    -- Left - 2 * Middle + Right
    S1x3 -> (rect i0 j0 i1 j') - 2 * (rect i0 j' i1 j'') + (rect i0 j'' i1 j1 )
      where
        deltaJ = (j1 - j0) `div` 3
        j'  = j0 + deltaJ
        j'' = j0 + deltaJ + deltaJ
  where
    -- Darkness of rectangle with BL, TR corners; (i0,j0); (i1,j1) respectively
    rect i0 j0 i1 j1 = idx i0 j0 + idx i1 j1 - idx i0 j1 - idx i1 j0
    idx a b = ii ! (Z :. a :. b)

-- Find Threshold given weights and labels to minimize 1/0 error of Haar Filter
type Polarity = Bool
type Threshold = Float
type Error = Float

findThreshold :: FilterParams ->
                 [WeightedPoint IntegralImage] ->
                 (Threshold, Polarity, Error)
{- Find the threshold and polarity that minimises error.
 - Threshold, Polarity and FilterParams together defines a Haar classifier which
 - works as follows: if polarity and (calculateFeature fp ii) > threshold
 - then classify ii as a face else as a background. -}
findThreshold fp wps = (th, p , err)
  where
    -- We want to maximise by polarity p, and threshold th;
    -- p * correctness = sum (weight | score < th) - sum (weight | score > th)
    -- Note that: (correctness @ th) + total = -2 {w | s < th}
    -- where total = sum {weight}
    th =  fromIntegral (scores !! idx + scores !! (idx+1)) / 2
    p = scr < 0 -- mostly backgrounds below threshold
    err = 1.0 - abs (2 * scr)
    -- Scan accross sortedWeighedScores consider threshold in the gaps
    -- cumsumWeights[i] == .3 means net weight below threshold is 30% faces
    -- Therefore as sum(Weights) == 0,  60% are correctly classified.
    (w', scr, idx) = maximumBy csw wsi
      where
        csw a b = compare (snd' a) (snd' b)
        snd' (weight, cumsumWeight, index) = cumsumWeight
        wsi = zip3 weights cumsumWeights [0..]
    cumsumWeights = scanl1 (+) weights
    (weights, scores) = unzip col
    -- Collapse identical scores and add weights
    col = map combine $ groupBy (\(w1,s1) (w2,s2) -> s1 == s2) sWeighedScores
      where
        combine group = let (w,s) = unzip group in (sum w, head s)
    sWeighedScores = sortOn snd weighedScores
    -- Calculate Weights and Scores
    weighedScores = map weighScores wps
    weighScores wp = (weigh wp ,calculateFeature fp $ point wp)
    weigh wp = if label wp then weight wp else - weight wp

stump :: FilterParams -> Polarity -> Threshold -> IntegralImage -> Bool
stump params pol th ii = pol && (fromIntegral score > th)
  where
    score = calculateFeature params ii

findPredictorAndError :: [WeightedPoint a] -> (IntegralImage -> Double,Error)
findPredictorAndError distribution = undefined
