{- Short Desc
   Long Desc
-}
module HaarFilter where

import Adaboost              (WeightedPoint, weight, label, point)
import Data.Array.Repa       hiding (map, (++))
import Data.List             (sortOn, groupBy, maximumBy, minimumBy)

type IntegralImage = Array U DIM2 Int


data FilterShape  = S1x1 | S1x2 | S2x1 | S2x2 | S3x1 | S1x3 deriving (Show, Eq)
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

findThreshold :: (a -> Int) ->         -- calculate feature value
                  [WeightedPoint a] ->  -- Weighted points
                  (Threshold, Polarity, Error)
{- Find the minimum 1/0 error by choice of Threshold and Polarity.
 - This allows a numeric feature score to become a simple binary classifier.
 - If (score > threshold) == (polarity == True) then classify as True ie group
 - A, Face, etc. Otherwise classify as False, group B, Background, etc. -}
findThreshold scoreFeature wpts = (thr, pol, err)
  where
    n = length wpts
    -- scoreGroups :: [(score,[(weight, label)])]
    scoreGroups = map fixGroup $ groupBy (\a b -> fst a == fst b) sortedScores
      where
        fixGroup sgrp = let (s, wl) = unzip sgrp in (head s, wl)
        sortedScores = sortOn fst $ map scorePoint wpts
        scorePoint wpt = (fromIntegral $ scoreFeature $ point wpt,
                          (weight wpt, label wpt))
    -- let Threshold be between scores in
    -- accA = sum(weight | score < Threshold, label = True)
    -- accB = sum(weight | score < Threshold, label = False)
    nextThreshold (accA, accB) (score, weightAndLabel) = (accA', accB')
      where
        accA' = accA + (sum $ map fst $ filter snd weightAndLabel)
        accB' = accB + (sum $ map fst $ filter (not.snd) weightAndLabel)
    acc = scanl nextThreshold (0,0) scoreGroups
    (totA, totB) = last acc
    -- To calculate Error from accA/accB if Polarity = T/F, note that:
    -- error(th,pol=T) = sum(w |s<th, l=T) + sum(w | s>th, l=F)
    --                 = sum(w |s<th, l=T) + sum(w|l = F) - sum (w | s<th, l=F)
    --                 = accA + totB - accB
    -- Fold to find the minimum Threshold index, error and polarity
    (err, i, pol) = foldl bestErr minThErr $ zip acc [0..]
      where
        bestErr (min_error, min_i, p) ((accA,accB), i)
          | e == min_error = (min_error, min_i, p)
          | e == errA      = (errA, i, True)
          | e == errB      = (errB, i, False)
          where
            errA = accA + totB - accB
            errB = accB + totA - accA
            e = minimum [min_error, errA, errB]
        -- case where best threshold < min scores
        minThErr = if totA > totB then (totA, -1, True) else (totB, -1, False)
    -- get minimum threshold from minimum threshold index
    thr | i == -1     = head scores' - 1 -- before minimum score
        | i == n - 1  = last scores' + 1 -- after maximum score
        | otherwise   = (scores' !! i + scores' !! (i-1)) / 2
      where scores' = map fst scoreGroups



stump :: FilterParams -> Polarity -> Threshold -> IntegralImage -> Bool
stump params pol th ii = pol && (fromIntegral score > th)
  where
    score = calculateFeature params ii


findBestPredictor :: [FilterParams] ->
                     [WeightedPoint IntegralImage] ->
                     (IntegralImage -> Bool)
{- Find the best FilterParams from a list. Embarassingly parallelizable. -}
findBestPredictor distribution = undefined
