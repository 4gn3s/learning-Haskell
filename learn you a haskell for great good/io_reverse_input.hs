reverseWords :: String -> String
reverseWords [] = []
reverseWords (x:xs) = (reverseWords xs) ++ [x]

reverseWords' :: String -> String
reverseWords' = unwords . map reverse . words
--first split the words into a list, then reverse each elem, then join them into a string again

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
