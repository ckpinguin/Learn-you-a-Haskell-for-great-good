import Data.List
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction (x:y:ys) "/" = return ((y / x):ys)
foldingFunction (x:y:ys) "^" = return ((y ** x):ys)
foldingFunction (x:xs)   "ln" = return ((log x):xs)
foldingFunction (x:xs)   "log" = return ((logBase 10 x):xs)
foldingFunction (xs)     "sum" = return ([sum xs])

foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result