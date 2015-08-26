import Data.Map (toList, fromListWith)
import Data.Function (on)
import Data.List (sortBy)
import Data.Char (intToDigit)

--hopscotch

getNth :: Int -> [a] -> [a]
getNth i xs = [x | (j, x) <- zip [1..] xs, j `mod` i == 0]

skips :: [a] -> [[a]]
skips xs = [getNth i xs | (i, x) <- zip [1..] xs]

test1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
test2 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
test3 = skips [1] == [[1]]
test4 = skips [True,False] == [[True,False], [False]]
--test5 = skips [] == []

--local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x,y,z) <- zip3 ((xs !! 0) : xs) xs (drop 1 xs), x<y && y>z]

test5 = localMaxima [2,9,5,6,1] == [9,6]
test6 = localMaxima [2,3,4,1,5] == [4]
test7 = localMaxima [1,2,3,4,5] == []

--histogram

-- occurences :: Ord a => [a] -> [(a, Int)]
-- occurences xs = toList $ fromListWith (+) [(x, 1) | x <- xs]

-- sortedoccurences :: Ord a => [a] -> [(a, Int)]
-- sortedoccurences xs = reverse $ sortBy (compare `on` snd) $ occurences xs

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

counts :: Eq a => [a] -> [a] -> [Int]
counts elements xs = map (flip count xs) elements

makeline :: [Int] -> Int -> String
makeline xs n = [if x >= n then '*' else ' ' | x <- xs]

histogram :: [Integer] -> String
histogram xs = lines ++ (replicate 10 '-') ++ "\n" ++ (map intToDigit [0..9]) ++ "\n"
  where
    lines = unlines $ map (makeline cs) (reverse [1.. maximum cs])
    cs = counts [0..9] xs

test8 = putStr $ histogram [1,1,1,5]
test9 = putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
