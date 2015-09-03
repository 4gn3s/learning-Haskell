import Data.Char (toUpper)

main = do
    putStrLn "Whats your name?"
    name <- getLine
    putStrLn "whats your favourite ice cream flavour?"
    ice <- getLine
    let bigName = map toUpper name
        bigIce = map toUpper ice
    putStrLn $ "hey " ++ bigName ++ ", welcome, i like " ++ bigIce ++ " too"


