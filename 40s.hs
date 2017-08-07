

-- 46
{-  Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
    (for logical equivalence) which succeed or fail according to the result of
    their respective operations; e.g. and(A,B) will succeed, if and only if
    both A and B succeed.
-}
not' :: Bool -> Bool
not' True  = False
not' False = True

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True  True = True
and' _     _    = False

or' False False = False
or' _      _    = True

nor'  a b = not' $ or'  a b
nand' a b = not' $ and' a b

xor' True  False = True
xor' False True  = True
xor' _     _     = False

impl' a b = (not' a) `or'` b

equ' True  True  = True
equ' False False = True
equ' _     _     = False

{-  Now, write a predicate table/3 which prints the truth table of a given
logical expression in two variables. -}

table :: (Bool -> Bool -> Bool) -> IO()
table f = printCell cells
    where
        cells  = map (\(a,b)->(a,b,f a b)) inputs
        inputs = [(a,b)|a<-[True,False], b<-[True,False]]
        printCell = mapM_ print

main = do {
    table (\a b -> (and' a (or' a b)))
}
