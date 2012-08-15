import Control.Monad

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
