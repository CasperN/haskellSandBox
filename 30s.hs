import Data.List (group)

-- 31
{- Note that besides 3,3 primes are of the form 6k +- 1 -}
isPrime :: Int -> Bool
isPrime n
    |n `elem` [2,3,5] = True
    |n `mod` 2 == 0 = False
    |n `mod` 3 == 0 = False
    |n `mod` 5 == 0 = False
    |n < 35 = True
    |otherwise = all foo [6,12.. sqrt' n]
    where
        sqrt' = ceiling . sqrt . fromIntegral
        foo k = (n `mod` (k-1) /= 0) && (n `mod` (k+1) /= 0)

-- 32
gcd' :: Integral a => a -> a -> a
gcd' x y
    | x < y = gcd'' y x
    | x > y = gcd'' x y
    | otherwise = x
    where -- we know a > b
        gcd'' a b
            | n == 0 = b
            | otherwise = gcd'' b n
            where
                n = mod a b

-- 33
coPrime :: Integral a => a -> a -> Bool
coPrime x y = 1 == gcd x y

-- 34
{- Euler's totient function is the number of integers r
 - such that 1<=r<=m and r is coprime to m -}
totient :: Int -> Int
totient n = length [x | x<-[1..n], gcd n x == 1]

-- 35
primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n
    | isPrime n = [n]
    | otherwise = k : primeFactors (n `div` k)
    where k = head [x | x  <- [2,3..], n `mod` x == 0]

-- 36
primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult x = map (\y-> (head y, length y)) ( group ( primeFactors x))

-- 37 Toitent but using prime factors
toitent' :: Int -> Int
toitent' n = product [(p-1)*p^(c-1) | (p,c) <-primeFactorsMult n]

-- 38 # Just comparing 34, 37
{- Given gcd is O(n) 34 is O(n^2) while 37 is O(n) -}

-- 39 Given a lower and upper limit, construct a list of prime numbers in that range
primeR :: Int -> Int -> [Int]
primeR l u = filter isPrime [l..u]

-- 40 every positive even integer greater than two is the sum of two primes
golbach :: Int -> (Int, Int)
golbach n = head [(a,b) | a <- primeR 2 (n-2), let b=n-a, isPrime b]

-- 41
golbachList :: Int -> Int -> [(Int, Int, Int)]
golbachList lb ub = map helper [lb,lb+2..ub]
    where
        helper k = let (p1,p2) = golbach k in (k,p1,p2)

main :: IO()
main = do {
    print $ isPrime 35;
    print $ primeFactors 315;
    print $ primeFactorsMult 315;
    print $ totient 25;
    print $ toitent' 25;
    print $ primeR 10 100;
    print $ golbach 22;
    print $ golbachList 10 20;
}
