
mergeSort :: (Ord a) => [a] -> [a]

mergeSort [x] = [x]
mergeSort []  = []
mergeSort x = merge (mergeSort x1) (mergeSort x2)
  where (x1,x2) = splitAt (length x `div` 2) x

merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b     = a : merge as (b:bs)
  | otherwise = b : merge (a:as) bs

main = print( mergeSort [200,25,1,3,5,2] )
