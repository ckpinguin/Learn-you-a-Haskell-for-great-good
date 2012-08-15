import Data.Char

main = do
    putStrLn "Hello, waddup?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock da house!")
    putStrLn ("Now, what's the name of your cat?")
    catName <- getLine
    let bigName = map toUpper name
        bigCat = map toUpper catName
    putStrLn $ "Whoa " ++ bigName ++ " your lil sucka is " 
                       ++ bigCat ++ " and you feed it for free!"

