{- Simple classifier of 64*64 sized grey images
-}
module HaarFilter where

import Adaboost              (WeightedPoint, weight, label, point, predictorError)
import Data.Array.Repa       hiding (map, (++))
import Data.List             (sortOn, sortBy, groupBy, foldl',foldl1', minimumBy)
import Data.Ord              (comparing)
import System.Random         (mkStdGen, randoms)

import Debug.Trace

type IntegralImage = Array U DIM2 Int
data FilterShape  = S1x1 | S1x2 | S2x1 | S2x2 | S3x1 | S1x3 deriving (Show, Eq)
data FilterWindow = Window Int Int Int Int deriving (Show)
data FilterParams = Filter {shape :: FilterShape, window :: FilterWindow}
  deriving (Show)


{-# ANN calculateFeature "HLint: ignore Redundant bracket" #-}
calculateFeature :: FilterParams -> IntegralImage -> Int
{- Image subtracts different sections of a detector window (Haar Feature)
 -}
calculateFeature (Filter {shape = sh , window = Window i0 j0 i1 j1}) ii =
  let
    -- Darkness of rectangle defined by BL, TR corners; (i0,j0); (i1,j1)
    idx a b = ii ! (Z :. a :. b)
    rect i0 j0 i1 j1 = idx i0 j0 + idx i1 j1 - idx i0 j1 - idx i1 j0
  in case sh of
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
    S1x3 -> (rect i0 j0 i1 j') - 2 * (rect i0 j' i1 j'') + (rect i0 j'' i1 j1)
      where
        deltaJ = (j1 - j0) `div` 3
        j'  = j0 + deltaJ
        j'' = j0 + deltaJ + deltaJ


type Polarity = Bool
type Threshold = Float
type Error = Float

findThreshold :: [WeightedPoint a]    -- Weighted points
              -> (a -> Int)           -- calculate feature value
              -> (Threshold, Polarity, Error)
{- Find the minimum 1/0 error of a HaarFilter by choosing Threshold & Polarity.
 - This allows a numeric feature score to become a simple binary classifier.
 - If (score > threshold) == (polarity == True) then classify as True ie group
 - A, Face, etc. Otherwise classify as False, group B, Background, etc.
 -}
findThreshold wpts scoreFeature = (thr, pol, err)
  where
    n = length wpts
    -- scoreGroups :: [(score,[(weight, label)])]
    scoreGroups = map fixGroup $ groupBy (\a b -> fst a == fst b) sortedScores
      where
        fixGroup sgrp = let (s, wl) = unzip sgrp in (head s, wl)
        sortedScores = sortOn fst $ map scorePoint wpts
        scorePoint wpt = (fromIntegral $ scoreFeature $ point wpt,
                          (weight wpt, label wpt))
    -- accA = sum(weight | score < Threshold, label = True)
    -- accB = sum(weight | score < Threshold, label = False)
    nextThreshold (accA, accB) (score, weightAndLabel) = (accA', accB')
      where
        accA' = accA + sum( map fst $ filter snd weightAndLabel)
        accB' = accB + sum( map fst $ filter (not.snd) weightAndLabel)
    acc = scanl nextThreshold (0,0) scoreGroups
    (totA, totB) = last acc
    -- To calculate Error from accA/accB if Polarity = T/F, note that:
    -- error(th,pol=T) = sum(w |s<th, l=T) + sum(w | s>th, l=F)
    --                 = sum(w |s<th, l=T) + sum(w | l=F) - sum(w | s<th, l=F)
    --                 = accA + totB - accB
    -- Fold to find the minimum Threshold index, error and polarity
    (err, i, pol) = foldl' bestErr minThErr $ zip acc [0..]
      where
        -- Case where best threshold < min scores
        minThErr = if totA > totB then (totA, -1, True) else (totB, -1, False)
        -- Consider Error / Polarity b/w each unique scores
        bestErr (min_error, min_i, p) ((accA,accB), i)
          | e == min_error = (min_error, min_i, p)
          | e == errA      = (errA, i, True)
          | e == errB      = (errB, i, False)
          where
            errA = accA + totB - accB
            errB = accB + totA - accA
            e = minimum [min_error, errA, errB]
    -- get minimum threshold from minimum threshold index
    thr | i == -1     = head scores' - 1 -- before minimum score
        | i == n - 1  = last scores' + 1 -- after maximum score
        | otherwise   = (scores' !! i + scores' !! (i+1)) / 2
      where scores' = map fst scoreGroups


stump :: FilterParams -> Polarity -> Threshold -> IntegralImage -> Bool
{- Decision stump. Turns an integral feature into a binary classifier -}
stump params pol th ii = pol && (fromIntegral score > th)
  where
    score = calculateFeature params ii


getRandomFilterParams :: Int -> Int -> [FilterParams]
{- Given a random seed, and n, generate n random FilterParams
 - A Filter param is a filter shape and two points defining a window
 -}
getRandomFilterParams seed n = take n $ allFP `sortWith` randomOrder
  where
    sortWith a b = map fst $ sortBy (comparing snd) $ zip a b
    randomOrder = randoms $ mkStdGen seed :: [Int]
    minWindowSize = 8
    allFP = [Filter sh (Window i0 j0 i1 j1) |
             sh <- [S1x1,S2x1,S1x2,S1x3,S3x1],
             i0 <- [0,2..62],
             j0 <- [0,2..62],
             i1 <- [i0 + minWindowSize, i0 + minWindowSize + 2 .. 63],
             j1 <- [j0 + minWindowSize, j0 + minWindowSize + 2 .. 63]]


findBestPredictor :: [FilterParams]                -- Potential filters
                  -> [WeightedPoint IntegralImage] -- Data
                  -> (IntegralImage -> Bool)       -- Best classfier & error
{- Find the best FilterParams from a list... by trying all of them and taking
 - the best. In particular the "map" is embarassingly parallelizable.
 -}
findBestPredictor filterParams distribution = if e < 0.5 then bestPred else undefined
  where
    e = traceShowId bestErr
    (bestPred, bestErr) = minimumBy (comparing snd) $ map toStumpErr filterParams
    toStumpErr fp = (s, predictorError distribution s)
      where
        s = stump fp p th
        (th, p, err) = findThreshold distribution $ calculateFeature fp
