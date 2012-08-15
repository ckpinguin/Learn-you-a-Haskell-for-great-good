import Data.List -- used for qsort2
import Data.Char
import qualified Data.Map as Map

doubleMe n = n + n
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
    then x
    else x*2

jimO'Brian = 12

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: Int -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, no luck this time, pal!"

rightTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]
rightTriangles2 = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]

-- Using a sequence and polymorphic functions
factorial1 :: Integer -> Integer
factorial1 n = product [1..n]

-- A Bit more effective (but limited with arch's int)
-- Using pattern matching
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Argument structure pattern matching
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- More Pattern matching / binding
first :: (a, b, c) -> a
first (x, y, z) = x -- This might be less efficient than the book's version
                    -- see below

-- Pattern binding (tuples)
second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Own implementation of head
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, buggar!" -- Really?
-- Why not return just an empty list?
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty sring, bugger!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ " and the rest would be: " ++ xs ++ "."

-- Guards
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat some stuff!"
    | bmi <= 25.0 = "You're quite 'normal', whatever that means..."
    | bmi <= 30.0 = "You're adipositous or just plain fat."
    | otherwise = "You're a whale or a house, congratulations."

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT 

-- Using where blocks for kind of local memoization (only for guard's context!)
bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
    | bmi <= 18.5 = "You're underweight, eat some stuff!"
    | bmi <= 25.0 = "You're quite 'normal', whatever that means..."
    | bmi <= 30.0 = "You're adipositous or just plain fat."
    | otherwise = "You're a whale or a house, congratulations."
    where bmi = weight / height ^ 2

-- Pattern matching in where blocks
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- functions in where blocks
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let expressions
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

-- using pattern matching in let expressions to break tuples into their
-- components and bind those components to names
-- (let (a, b, c) = (1, 2, 3) in a+b+c) * 100

-- let in list comprehensions
-- the (w,h) <- xs part is called the generator (bmi var is not available there)
calcBmisl :: [(Double, Double)] -> [Double]
calcBmisl xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- Recursion exercises
-- Fibonacci is very simple but no tail recursion of course!
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- List walking recursion for maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list, punk!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- We use guards for "decreasing counter" type recursion
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- A bit more complex (at least it looks so...), it's using a guard
-- (but without an otherwise part) and arguments pattern,
-- that's why it's a bit creepy
-- It's like replicate1 as it has a decreasing counter, but it has more
-- checking for zero argument / empty list.
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Augmenting (consing) recursion: reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] -- no direct consing possible of course
-- I'm starting to like this clear expressive power of Haskell more and more...

-- Even recursions without anchors for infinite lists are totally legal
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- zip
-- using the cool patterns to decompose after base case checks
zip' :: [a] -> [b] -> [(a,b)] -- list of tuples
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


-- Currying examples
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- Partially applied functions (currying)
compareWithHundred :: Int -> Ordering
--compareWithHundred x = compare 100 x
-- This is equivalent to above! 
compareWithHundred = compare 100

-- Partially applied infix functions (using sections)
dividebyTen :: (Floating a) => a -> a
dividebyTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Applicative / returning functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- Find largest number below 100000 that is divisible by 3829
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Collatz chains
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (div n 2)
    | odd  n = n : chain (n * 3 + 1)

-- Search Collatz chain (starting below 100) with a length greater than 15
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Using currying with lambdas just for example (the original addThree above is 
-- much more clearer anyways)
addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z
-- as () are right-associative anyway, you can omit parantheses here, every-
-- thing on the right of -> will be treated as if it belongs to the lambda
-- expr on the respective left
--addThree' = (\x -> (\y -> (\z -> x + y + z))) 

flip'' :: (a -> b -> c) -> b -> a -> c
-- This is a bit more verbose about producing a new function than using the 
-- above version (f x y = f y x)
flip'' f = \x y -> f y x

-- Folding examples (shortcut for traversing lists with applicative functions)
sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
-- even shorter taking currying into account (functions like foo a = bar b a 
-- can be rewritten as foo = bar b thanks to currying).
-- sum' xs = foldl (+) 0 xs
-- and even shorter!
sum' = foldl (+) 0

-- Right folds are used to build up lists with : (as we know, this is done
-- from the right: for example [4] is syntactic sugar for 4:[] etc.)
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
-- we could also do it from the left with ++, but ++ is slower than :
--map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Remark: Only right folds can work with infinite lists (and that only when 
-- the binary function does not need to go through the whole list like &&)!
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []
-- also possible:
reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- scanl/r examples
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Pointer-Free style example
-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- becomes:
fn = ceiling . negate . tan . cos . max 50

-- Using Data.List import
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)

encode :: Int -> String -> String
--encode offset msg = map (\c -> chr $ ord c + offset) msg
-- for composition fans:
encode offset msg = map (chr . (+ offset) . ord) msg

-- decode is easy because encode is symmetric
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- examples of association lists / dictionaries
-- This one crashes too easy (if the list is empty or so)
--findKey :: (Eq k) => k -> [(k, v)] -> v
--findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- This one's better
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs
-- The folding version (it's usually more clear and readable to use foldr, when
-- having the classic list recursion pattern)
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
          [("betty", "555-4393")
          ,("bunny", "333-4429")
          ,("hopsie", "221-3209")
          ,("hopsie", "493-3211")
          ,("karfunkl", "998-3954")
          ,("zaphod", "233-3422")
          ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

intBook = Map.map string2digits phoneBook
--phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- This doesn't work as in the book:
--phoneBookToMap xs = Map.fromListWith add xs
--    where add number1 number2 = number1 ++ ", " ++ number2
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
