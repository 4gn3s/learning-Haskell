import Prelude hiding (or, minimum, lookup)

triangle :: Int -> Int
triangle n = sum [0..n]

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

minimum :: [Int] -> Int
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys) = if x==y then 1 + count x ys else count x ys

lookup :: (Eq a) => a -> [(a,b)] -> b
lookup key [] = error "key not in dict"
lookup key (pair:dict) = if key== fst pair then snd pair else lookup key dict

euclid :: Int -> Int -> Int
euclid x y
	| x == y = x
	| x > y  = euclid (x-y) y
	|otherwise = euclid x (y-x)

test1 = euclid 6 27
-- > 3

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:zs) = [f x ] ++ [g y] ++ altMap f g zs

test2 = altMap (+10) (+100) [1,2,3,4,5]
-- > [11,102,13,104,15]

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then x*2 - 9 else x*2

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble (reverse xs)) `mod` 10 == 0

test3 = luhn [1,7,8,4]
-- > True
test4 = luhn [4,7,8,3]
-- > False
