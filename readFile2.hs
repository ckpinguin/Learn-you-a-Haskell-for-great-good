import System.IO

main = do
    withFile "test.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)