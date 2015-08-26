--wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x-2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

test1 = fun1 [1,4,2,6,2,7,3,8,5,8,9,9,3] == fun1' [1,4,2,6,2,7,3,8,5,8,9,9,3]

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate step
  where
    step n = if even n then n `div` 2 else 3 * n + 1

test2 = fun2 19 == fun2' 19

--binary trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode y (Node h l x r)
  | height l <= height r = let
                            new = insertNode x l
                          in Node (height new + 1) new y r
  | otherwise = let
                  new = insertNode x r
                in Node (height new + 1) l y new

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

--more folds
xor' :: [Bool] -> Bool
xor' = odd . length . filter id

xor :: [Bool] -> Bool
xor = foldl single_xor False
  where
    single_xor True True = False
    single_xor False False = False
    single_xor _ _ = True

test3 = xor [False, True, False] == True
test4 = xor [False, True, False, False, True] == False
test5 = xor [False, True, False, False, True, True] == True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

--finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) filtered
  where
    filtered = filter (\x -> x `notElem` to_remove) [1..n]
      where
        to_remove = [x + y + 2 * x * y | y <- [1..n], x <- [1..n], x + y + 2 * x * y <= n]
