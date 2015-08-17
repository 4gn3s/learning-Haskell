import Prelude hiding ((.))
twice :: (a -> a) -> a -> a
--high order function- takes a function as 1st argument
twice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs) = if f x then x : filter'' f xs else filter'' f xs

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' f [] = []
filter''' f (x:xs)
	| f x = x : filter''' f xs
	| otherwise = filter''' f xs

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sum'' = foldr' (+) 0

product'' = foldr' (*) 1

and'' = foldr' (&&) True

-- product' [1,2,3]
-- foldr' (*) 1 [1,2,3]
-- foldr' (*) 1 (1:(2:(3:[])))
-- zamieniamy każdy : na f (czyli *) i każdy [] na v (czyli 1)
-- (1*(2*(3*1)))
-- 6

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' = foldr' (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' = foldr' (\x xs -> xs ++ [x]) []

map''' f = foldr' (\x xs -> f x : xs) []

filter'''' f = foldr' (\x xs -> if f x then x : xs else xs) []

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

odd' :: Int -> Bool
odd' = not . even

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (a:as) = f a && all' f as

all'' f = foldr' (\x xs -> f x && xs) True

all''' f xs = and [f x | x <- xs]

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile f [] = []
takewhile f (x:xs) 
	| f x = x : takewhile f xs
	| otherwise = []

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile f [] = []
dropwhile f (x:xs)
	| f x = dropwhile f xs
	| otherwise = x : xs

mapl f = foldl (\ xs x -> xs ++ [f x]) []

dec2int = foldl (\ x y -> 10 * x + y) 0
-- dec2int [1,2,3,4] = 1234

compose :: [([a] -> [a])] -> ([a] -> [a])
compose = foldl (.) id

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \ x y -> f(x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \ (x, y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' = unfold null (take 8) (drop 8)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f
