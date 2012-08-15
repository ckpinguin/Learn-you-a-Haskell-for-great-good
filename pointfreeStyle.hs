-- Don't confuse "pointfree" with "free of ." ;-) With a point, the
-- arguments are meant: f x = x + 1 Here x is an arbitrary 'point'
-- (la point is a value).

-- A very nice example of how pointfree style (no function 
-- arguments / using function composition) can make things elegant
mem :: Eq a => a -> [a] -> Bool
mem = any . (==)

-- Even more simple an incrementor
inc :: Num a => a -> a
inc = (+ 1)