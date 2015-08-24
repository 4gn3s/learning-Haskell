zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

testZipWith' = zipWith' (+) [1..4] [5..8]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

testMap' = map' (replicate 3) [3..6]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

testFilter' = filter' even [1..10]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' (filter' (<=x) xs) ++ [x] ++ quicksort' (filter' (>x) xs)

-- find the largest number under 100,000 that's divisible by 3829
largestDivisable :: Integer
largestDivisable = head (filter p [1000000, 999999 ..])
  where
    p x = x `mod` 3829 == 0

sumOddSquaresSmaller :: (Integral a) => a -> a
sumOddSquaresSmaller n = sum (takeWhile (<n) (filter odd (map (^2) [1..])))

sumOddSquaresSmaller' :: (Integral a) => a -> a
sumOddSquaresSmaller' n = sum (takeWhile (<n) [n^2 | n <- [1..], odd (n^2)])

collatz :: (Integral a) => a -> a
collatz n
  | n <= 0 = error "n should be >= 0"
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSequence :: (Integral a) => a -> [a]
collatzSequence a
  | a == 1 = [1]
  | otherwise = a : collatzSequence ca
      where ca = collatz a


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n =  n:chain (n `div` 2)
  | odd n  =  n:chain (n*3 + 1)

--how many numbers between 1 and 100 have a chain longer than 15
testCollatzLengths = sum [1 | x<-[1..100], length (collatzSequence x) > 15]

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
                where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--foldl: folds list from left, using the current accumulator and lists head
--parameters:
--1. binary function with 2 arguments: the accumulator and the lists head; it creates the new accumulator \acc x -> ...
--2. a starting value (initial accumulator value)
--3. a list to fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False
elem' xs a = foldl (\acc x -> if x==a then True else acc) False xs

--foldr: folds from right
--the binary function parameters are reversed: \x acc -> ...

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

--can be also implemented as:
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
--but the ++ operation is much more expensive than the : operation

--foldl1 and foldr1 are similar, but don't take the initial value
--they just take the first or last element of the list

sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+)

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x >= acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)--(\x acc -> x * acc)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

--scanr, scanl: like foldr, foldl but they report all intermediate results in a list
--also scanr1, scanl1

-- $ function application right-associative
--f a b c means (((f a) b) c)
--instead of writing sum (map sqrt [1..100]) we can just write sum $ map sqrt [1..100]
--sqrt (3+4+9) becomes sqrt $ 3+4+9
--sum (filter (>10) (map (*2) [1..10])) can be rewritten as sum $ filter (>10) $ map (*2) [1..10]
--
-- . function composition
--(f*g)(x)=f(g(x))
--to convert all numbers to negative we can write:
--map (\x -> negate (abs x)) [5, -3, -5, 1, -4]
--or cleaner:
--map (negate . abs) [5, -3, -5, 1, -4]

numLongChains'' :: Integer
numLongChains'' = sum . takeWhile (<100) . filter odd . map (^2) $ [1..]
