{- This program builds a face detector replicating the Viola Jones paper.

It classifies 64*64 pixel grey images as faces or backgrounds using a cascade of
adaboosted Haar Filters. Its super fast! (but not to train)

TODO FLAGS: faceDir, backDir, nFeatures, saveClassifier, testImage
-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
-- Import from universe
import Prelude               hiding (traverse)
import Control.Monad         (mapM)
import Data.List             (transpose)
import System.Directory      (getDirectoryContents)
import System.FilePath       (takeExtension)
import Data.Word             (Word8)
import Codec.Picture.Repa    (imgData,readImage, RGB, Img)
import Data.Array.Repa       hiding (map, (++), zipWith, transpose)
-- Import from App
import HaarFilter
import Adaboost

faceDir = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/faces/"
backDir = "/Users/Casper/Dropbox/School Stuff/Past classes/CMSC 254/ML hw4/background/"

main :: IO ()
main = do
  print "Reading faces..."
  faces <- readDirToIntegralImages faceDir
  print "Reading backs..."
  backs <- readDirToIntegralImages backDir
  let wi = weighAndLabelPoints faces backs
  let fps = getRandomFilterParams 5 50
  let s = findBestPredictor fps wi
  print $ predictorError wi s

  --
  -- let fp = Filter {shape = S2x1, window =  Window 30 15 50 40}
  --
  -- let (th, pol, err) = findThreshold  wi (calculateFeature fp)
  -- print $ "findThreshold -> (th,pol,err)"
  -- print $ (th,pol,err)
  -- let s = stump fp pol th
  -- print $ "The error actually is"
  -- print $ predictorError wi s

  -- print "reading files"
  -- facefiles <- getJpgs faceDir
  -- faces <- mapM readImageToArray facefiles
  -- print "files read"
  -- let greyFaces = map greyImage faces
  -- print "faces greyed"
  -- let a = head greyFaces
  -- let b = toIntegralImage a

-- IO

readDirToIntegralImages :: String -> IO [IntegralImage]
readDirToIntegralImages dir = do
  files <- getJpgs dir
  imgs <- mapM readImageToArray files
  return $ map (toIntegralImage . greyImage) imgs


getJpgs :: String -> IO [String]
getJpgs dir = do
  files <- getDirectoryContents dir -- in new haskell its listDirectory
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
 - using an inefficient list based method because there is no repa scan :(
 -}
toIntegralImage rArray = fromListUnboxed (Z :. 64 :. 64) ii2
  where
    -- To Repa
    ii2 = map fromIntegral $ concat ii :: [Int]
    ii = cumsum2D elements2D
    -- Cumulative summation over both axis
    cumsum2D = transpose . cumsum . transpose . cumsum
    cumsum = scanl1 (zipWith (+))
    -- From Repa
    elements2D = [[ x | (idx,x) <- elements, idx`div`64 == row]| row <- [0..63]]
    elements = zip [0,1..] . map toInteger $ toList rArray
