import Control.Monad
import Data.Monoid

isBig :: Int -> (Bool, String)
isBig x = (x > 9, " Compared something to 9.")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, log) f = 
    let (y, newLog) = f x
    in (y, log ++ newLog)

-- Here the better version working with any monoid:
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f =
    let (y, newLog) = f x
    in  (y, log `mappend` newLog)