myLast x = head $ reverse x
myLast' = head . reverse

secondLast x = (reverse x)!!1


data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (elem x) = [x]
flatten (x:xs) = flatten x ++ flatten xs


compresser :: [a] -> [a]
compresser [] x = [x]
compresser acc x
    | last acc == x = acc
    | otherwise     = acc ++ [x]
compress = foldl compresser []

pack :: (Eq a) => [a] -> [[a]]

pack = foldr packer [] . map (\x->[x])
    where
        packer x [] = [x]
        packer x (y:ys)
            | head x == head y = (x++y) : ys
            | otherwise        =  x : y : ys



encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack

data ListItem a = Single a | Multiple Int a
    deriving Show
encode2 :: (Eq a) => [a] -> [ListItem a]
encode2 = map f . encode
    where
        f (1,x) = Single x
        f (n,x) = Multiple n x


main = do {
    print(encode2 "aaabbcdde" )
    print(secondLast [1..10])
    print( myLast [1,2,3,4,5])
}
