data ListItem a = Single a | Multiple Int a
    deriving Show


unpack :: [ListItem a] -> [a]
unpack = foldl (++) [] . map decode
    where
        decode (Single a)     = [a]
        decode (Multiple n a) = replicate n a



encodeDirect :: (Eq a) => [a] -> [(Int, a)]
encodeDirect = foldr h []
    where
        h x [] = [(1, x)]
        h x ((n,y):acc)
            | x == y    = (n+1,y):acc
            | otherwise = (1,x):(n,y):acc


slice :: [a] -> Int -> Int -> [a]
slice l i k = map dropcounter ( filter inRange ( zip [1..] l))
    where
        dropcounter (n,x) = x
        inRange (n,x) = i <= n && n <= k


rotate :: [a] -> Int -> [a]
rotate x 0 = x
rotate (x:xs) n | n > 0 = rotate (xs++[x]) (n-1)
rotate x n | n < 0 = reverse $ rotate (reverse x) (-n)

dropAt k l = (kth, a ++ b)
    |
        kth = dropAt !! k
        a = take (k-1) l
        b = drop (k+1) l

main = do {
    print( unpack [Single 2, Multiple 3 4, Multiple 4 2, Single 5])
    print(encodeDirect "aaabbcccdee")
    print( dropAt 5 [1..10])
}
