import Control.Monad.State

-- State without State Monad:
type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = let
    ((), newStack1) = push' 3 stack
    (a,  newStack2) = pop' newStack1
    in pop' newStack2


-- State with State Monad:
-- By using State type (a Monad), we get >>= for free and thus
-- can make easier stateful computations in statements like do
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip'' :: State Stack Int
stackManip'' = do
    push 3
    a <- pop
    pop

-- It's possible to do it even simpler!
stackManip :: State Stack Int
stackManip = do
    push 3 
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

popS :: State Stack Int
popS = do
    (x:xs) <- get
    put xs
    return x

pushS :: Int -> State Stack ()
pushS x = do
    xs <- get
    put (x:xs)

stackStuffS :: State Stack ()
stackStuffS = do
    a <- popS
    if a == 5
        then pushS 5
        else do
            pushS 3
            pushS 8