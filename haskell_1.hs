import Data.Char (isLower)

abs :: Int -> Int
abs x = if x >=0 then x else -x

sign :: Int -> Int
sign x = if x < 0 then -1 else
		 if x == 0 then 0 else 1

abs' :: Int -> Int
abs' x | x >=0 		= x
	   | otherwise  = -x

sign' :: Int -> Int
sign' x | x < 0 	= -1
        | x == 0	= 0
		| otherwise	= 1

not :: Bool -> Bool
not False = True
not True = False

and'' :: Bool -> Bool -> Bool
True `and''` a = a
False `and''` _ = False

head :: [a] -> a
head (a:_) = a

tail :: [a] -> [a]
tail (_:as) = as

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const' :: a -> b -> a
const' x = \_ -> x

odds n = map (\x -> 2*x+1) [0..n-1]

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

myzip :: [a] -> [b] -> [(a, b)]
myzip (a:as) (b:bs) = (a,b) : myzip as bs
myzip _ _ = []

pairs :: [a] -> [(a,a)]
pairs as = zip as (Main.tail as)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x'==x]

lowercaseCount :: String -> Int
lowercaseCount xs = length [x | x <- xs, isLower x]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <-t, k == k']

positions' :: (Eq a) => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..n])
	where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- xs `zip` ys]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides n x = n `mod` x == 0

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `divides` x]

factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n
	| n < 0 	= 0 -- dla ujemnych bedzie stackoverflow bo rekursja sie nie zatrzyma
	| otherwise = n * factorial (n-1)

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs 

length' :: [a] -> Int
length' [] 	   = 0
length' (_:as) = 1 + length' as

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (a:as) = reverse as ++ [a]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys) 

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
	where 
		smaller = [a | a <- xs, a <= x]
		larger  = [a | a <- xs, a > x]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

index :: [a] -> Int -> a --select nth element from the list
index (x:_) 0 = x
index (x:xs) n = index xs (n-1)
index [] _ = error "Can't select an element from an empty list"

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) a = (x == a) || contains xs a --if x == a then True else contains xs a

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

and' :: [Bool] -> Bool
and' (b:bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort as = merge (mergesort xs) (mergesort ys)
	where (xs, ys) = halve as
