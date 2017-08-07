{- Short Desc
   Long Desc
-}
module HaarFilter where

import Adaboost              (WeightedPoint)
import Data.Array.Repa       hiding (map, (++))

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
type Threshold = Int
type Error = Double


findThreshold :: FilterParams -> [WeightedPoint a] -> (Threshold, Polarity, Error)
findThreshold distribution = undefined

stump :: FilterParams -> Polarity -> Threshold -> IntegralImage -> Bool
stump params polarity threshold ii = polarity && (score > threshold)
  where
    score = calculateFeature params ii

findPredictorAndError :: [WeightedPoint a] -> (IntegralImage -> Double,Error)
findPredictorAndError distribution = undefined
