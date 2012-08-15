import Data.Ratio
import Data.Functor
import Data.List (all)


newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- Nested probability lists (like dependend probability)
situation1 :: Prob (Prob Char)
situation1 = Prob
    [(Prob [('a', 1%2),('b', 1%2)], 1%4)
    ,(Prob [('c', 1%2),('d', 1%2)], 3%4)
    ]

-- Flattening (i.e. join, but this name is already taken) means just 
-- multiplying dependent (inner / outer) probabilities with each other.
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2), (Tails, 1%2)]

unfairCoin :: Prob Coin
unfairCoin = Prob [(Heads, 9%10), (Tails, 1%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- unfairCoin
    return (all (==Tails) [a,b,c])