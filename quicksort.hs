import Data.List

-- quicksort ftw!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = -- simply take the first element as pivot
    let smallerOrEqual = [a | a <- xs, a <= x] -- left of pivot
        larger = [a | a <- xs, a > x] -- right of pivot
    in  quicksort smallerOrEqual ++ [x] ++ larger
-- This can be done with filter also
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quicksort' smallerOrEqual ++ [x] ++ quicksort' larger-- the famous 2-liner-quicksort (rosettacode.org)
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]
-- this is so expressive, I want to dance! Whoever tries to implement quicksort 
-- in c/c++/java/etc. knows how hard it is even to understand the code!
-- But here, we just say, what the algorithm should do, and that in the correct 
-- order from left to right. Most of the complexity comes from partitioning 
-- (generic) arrays of course and this is "just" a list, but the method is as
-- polymorphic as the generic arrays, as long as the types are of class Ord.
-- (This is called parametric polymorphism in functional languages).

-- And more efficient, only one comparison per element:
-- import Data.List -- (imports only at the beginning of the file!)
qsort' [] = []
qsort' (x:xs) = qsort' ys ++ x : qsort' zs where (ys, zs) = partition (< x) xs
