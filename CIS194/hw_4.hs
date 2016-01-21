import Data.List
-- exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs

fun1' = product . map (\x -> x - 2) . filter even

test_fun1 xs = fun1 xs == fun1' xs
test1 = test_fun1 [1..110]

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

-- TODO fun2'

-- exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

addToTree :: Tree a -> a -> Tree a
addToTree Leaf x = Node 0 Leaf x Leaf
addToTree (Node n Leaf v Leaf) x = Node (n+1) (addToTree Leaf x) v Leaf
addToTree (Node n Leaf v right) x = Node n (addToTree Leaf x) v right
addToTree (Node n left v Leaf) x = Node n left v (addToTree Leaf x)
addToTree (Node n left@(Node leftH _ _ _) v right@(Node rightH _ _ _)) x =
    case leftH - rightH of
        1 -> Node leftH left v (addToTree right x)
        0 -> Node (n+1) left v (addToTree right x)
        -1 -> Node rightH (addToTree left x) v right

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = addToTree (foldTree xs) x

foldTree' :: [a] -> Tree a
foldTree' = foldl addToTree Leaf

test2 = foldTree "ABCDEFGHIJ"
test3 = foldTree' "ABCDEFGHIJ"

-- exercise 3

-- nrOfTrues :: [Bool] -> Int -> Int
-- nrOfTrues [] n = n
-- nrOfTrues (x:xs) n = if x == True then (n+1) else n + nrOfTrues xs

xor :: [Bool] -> Bool
xor = odd . foldr (\x numTrue -> if x then numTrue + 1 else numTrue) 0

test4 = xor [False, True, False] == True
test5 = xor [False, True, False, False, True] == False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 *x + 1) $ [1..n] \\ toRemove n -- \\ is list difference

toRemove :: Integer -> [Integer]
-- start with [1..n] and remove all numbers of form i+j+2*i*j where:
-- 1 <= i <= j
-- 1 <= j <= floor (n/2)
-- i + j + 2*i*j <= n
toRemove n = [a | j <- [1..n `div` 2], i <- [1..j], let a = i + j + 2 * i * j, a <= n]
