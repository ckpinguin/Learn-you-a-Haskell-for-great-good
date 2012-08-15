import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction acc . words
    where acc = []
          foldingFunction (x:y:ys) "*" = (y * x):ys
          foldingFunction (x:y:ys) "+" = (y + x):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:y:ys) "^" = (y ** x):ys
          foldingFunction (x:xs) "ln" = (log x):xs
          foldingFunction (x:xs) "log" = (logBase 10 x):xs
          foldingFunction (xs) "sum" = [sum xs]
          foldingFunction xs numberString = read numberString:xs
