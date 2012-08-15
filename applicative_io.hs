import Control.Applicative

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myApplicativeAction :: IO String
myApplicativeAction = (++) <$> getLine <*> getLine