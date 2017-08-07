import System.Random
import Data.Ord (comparing)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys k = take k ys ++ [x] ++ drop k ys

-- 22
range :: Int -> Int -> [Int]
range j k = [j..k]

-- 23
randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n = do
    gen <- getStdGen
    return $ take n [xs!!k | k <- randomRs (0, length xs - 1) gen ]

-- 24
lotto :: Int -> Int -> IO [Int]
lotto n m = do
    gen <- getStdGen
    return $ take n (randomRs(0,m) gen)

-- 25
randomPermute :: [a] -> IO [a]
randomPermute [] = return []
randomPermute xs = do
        gen <- getStdGen
        k <- randomRIO (0, length xs - 1)
        return $ drop k xs ++ take k xs

-- 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _  = [[]]
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- 27
combinations' :: Int -> [a] -> [([a],[a])]
{- a list of combinations and unused combinations -}
combinations' 0 xs = [([],xs)]
combinations' _ [] = []
combinations' k (x:xs) = ts ++ ds
    where
        ts = [(x:used, unused) | (used,unused) <- combinations' (k-1) xs]
        ds = [(used, x:unused) | (used,unused) <- combinations' k xs]

group :: [Int] -> [a] -> [[[a]]]
{- List of list of disjoint lists of specified cardinality-}
group [] _ = [[]]
group (n:ns) xs = [used:others | (used,unused) <- combinations' n xs
                               , others <- group ns unused]

-- 28 sort a list of lists by increasing frequency of length of sublists
lfsort :: [[a]] -> [[a]]
lfsort = sortBy (comparing length)

-- 29


main = print $ group [1,2] ['a'..'g']
    --res <- randomPermute [1..10]
    --print $ res
