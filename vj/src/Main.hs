{- Short summary

Long description

stuck due to package reasons
dependances suck with friday and repa devil

-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude               hiding (traverse)
import Control.Monad         (mapM)
import Data.List             (transpose)
import System.Directory      (getDirectoryContents)
import System.FilePath       (takeExtension)
import Data.Word             (Word8)
import Codec.Picture.Repa    (imgData,readImage, RGB, Img)
import Data.Array.Repa       hiding (map, (++), zipWith, transpose)
-- App
import HaarFilter
import Adaboost
-- import Cumsum

ex = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/faces/face0.jpg"
ex2 = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/faces/class.jpg"

faceDir = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/faces/"
backDir = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/background/"

-- Main stuff

-- getDirectoryContents is old version which I need for JuicyPixels :/
listDirectory = getDirectoryContents



main :: IO ()
main = do
  print "Reading faces..."
  faces <- readDirToIntegralImages faceDir
  print "Reading backs..."
  backs <- readDirToIntegralImages backDir
  let wi = weighImages faces backs
  print "done"

  -- print "reading files"
  -- facefiles <- getJpgs faceDir
  -- faces <- mapM readImageToArray facefiles
  -- print "files read"
  -- let greyFaces = map greyImage faces
  -- print "faces greyed"
  -- let a = head greyFaces
  -- let b = toIntegralImage a

  -- TO DO: Cumulative sum
  -- do
  --   x <- computeUnboxedP $ head greyFaces
  --   print x


-- IO

readDirToIntegralImages :: String -> IO [IntegralImage]
readDirToIntegralImages dir = do
  files <- getJpgs dir
  imgs <- mapM readImageToArray files
  return $ map (toIntegralImage . greyImage) imgs


getJpgs :: String -> IO [String]
getJpgs dir = do
  files <- listDirectory dir
  return $ map (dir++) $ filter (\f -> ".jpg" == takeExtension f) files


readImageToArray :: String -> IO (Array D DIM3 Word8)
readImageToArray file = do
  e <- readImage file :: IO (Either String (Img RGB))
  let img = either (fail "no img") imgData  e
  return img


{-# INLINE greyImage #-}
greyImage :: Array D DIM3 Word8 -> Array D DIM2 Word8
greyImage img = traverse img collapse luminosity
  where
    collapse (Z :. i :. j :. _) = Z :. i :. j
    -- From haskell repa tutorial, grey an image via luminosity method
    luminosity f (Z :. i :. j ) = ceiling $ 0.21 * r + 0.71 * g + 0.07 * b
      where
        r = fromIntegral $ f (Z :. i :. j :. 0)
        g = fromIntegral $ f (Z :. i :. j :. 1)
        b = fromIntegral $ f (Z :. i :. j :. 2)


toIntegralImage :: (Source r Word8) => Array r DIM2 Word8 -> IntegralImage
{- Use IntegralImage to have O(1) calculation of HaarFeatures. This function is
   using an inefficient list based method because there is no repa scan :(    -}
toIntegralImage rArray = fromListUnboxed (Z :. 64 :. 64) ii2
  where
    -- To Repa
    ii2 = map fromIntegral $ concat ii :: [Int]
    ii = cumsum2D elements2D
    -- Cumulative summation over both axis
    cumsum2D x = transpose $ cumsum $ transpose $ cumsum x
    cumsum = scanl1 (zipWith (+))
    -- From Repa
    elements2D = [[ x | (idx,x) <- elements, idx`div`64 == row]| row <- [0..63]]
    elements = zip [0,1..] . map toInteger $ toList rArray


{-- Haar Filter: a simple feature to be combined dozens of times by Adaboost --}


{-------------------- Weigh and Label Images for Adaboost ---------------------}