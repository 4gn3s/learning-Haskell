import Data.Char
--getContents is an I/O action that reads everything from
--the standard input until it encounters an end-of-file characte
--getContents does lazy I/O

main1 = do
    contents <- getContents
    putStrLn $ map toUpper contents

main2 = do
    contents <- getContents
    putStrLn $ shortLinesOnly contents

--interact takes a function of type String -> String as a parameter
--and returns an I/O action that will take some input,
--run that function on it and then print out the function's result
main3 = interact shortLinesOnly

main = interact $ unlines . filter ((<10) . length) . lines

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result
