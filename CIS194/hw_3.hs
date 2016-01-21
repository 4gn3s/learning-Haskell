import Data.Function (on)
import Data.List (sortBy)
import Data.Char (intToDigit)

-- hopscotch
getEveryNth :: [a] -> Int -> [a]
getEveryNth xs n = [x | (i, x) <- zip [1..] xs, i `mod` n == 0]

skips :: [a] -> [[a]]
skips xs = [getEveryNth xs i | i <- take (length xs) [1..]]

test1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
test2 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
test3 = skips [1] == [[1]]
test4 = skips [True, False] == [[True, False], [False]]
test5 = null (skips [])

-- local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x,y,z) <- zip3 (head xs : xs) xs (tail xs), x < y && y > z]

test6 = localMaxima [2,9,5,6,1] == [9,6]
test7 = localMaxima [2,3,4,1,5] == [4]
test8 = null $ localMaxima [1,2,3,4,5]
test9 = localMaxima [2,3,4,1,5] == [4]

-- histogram

getCount xs x = length $ filter (==x) xs

occurences xs = [(a, getCount xs a) | a <- [0..9]]

sortedOccurences xs = sortBy (flip compare `on` snd) $ occurences xs

currentLine sortedOcs = let
                            line = [x | (x, y) <- sortedOcs, y>0]
                            afterwards = [(x,y-1) | (x,y) <- sortedOcs, y>0]
                         in
                            (line, afterwards)

getHistLine line = [if x `elem` line then '*' else ' ' | x <- [0..9]] ++ "\n"

getLines [] = ""
getLines sortedOcs = let (line, afterLines) = currentLine sortedOcs
                     in
                        if null line
                            then getLines afterLines
                            else getLines afterLines ++ getHistLine line

histogram :: [Integer] -> String
histogram xs = getLines (sortedOccurences xs) ++ replicate 10 '=' ++ "\n" ++ map intToDigit [0..9] ++ "\n"

test10 = putStr $ histogram [1,1,1,5]
test11 = putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
