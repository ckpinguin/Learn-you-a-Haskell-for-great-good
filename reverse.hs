main = do
    line <- getLine
    -- Hmm, here it starts looking very imperative...
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
-- But still this is functional, a "loop" with a recursive call

reverseWords :: String -> String
reverseWords = unwords . map reverse . words