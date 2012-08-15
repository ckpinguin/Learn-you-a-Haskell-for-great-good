import Control.Monad.Writer
import Data.Monoid

f `append` g = \xs -> f (g xs)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []


instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- More efficient using difference lists than using normal lists, because
-- things are appended (lists are only efficient when prepending! But with
-- difference lists we don't have to care.)
-- At least this is so, if you do the recursion call(s) _before_ doing the
-- tell (or other Writer stuff in IO)!
gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result


-- Example of appending lists when using logging stuff with recursive functions
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

finalCountDownSlow :: Int -> Writer [String] ()
finalCountDownSlow 0 = do
    tell ["0"]
finalCountDownSlow x = do
    finalCountDownSlow (x-1)
    tell [show x]