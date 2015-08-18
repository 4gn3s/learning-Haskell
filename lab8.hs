module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle n
    | n == 0 = 0
    | n > 0 = n + triangle (n - 1)
    | otherwise = error "n >=0"

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (x:xs) = if a == x then 1 + count a xs else count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

test_count_1 = count "Haskell" ["Java", "PHP", "Javascript", "C#"] == 0
test_count_2 = count 'e' "The quick brown fox jumped over the lazy dog." == 4

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

ex_4 = count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x)

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x <= 0 || y <= 0 = error "should be x>0 and y>0"
  | otherwise = if x == y then fst (x, y) else if x > y then euclid ((x-y), y) else euclid (x, (y-x))

test_euclid_1 = euclid (5, 7) == 1
test_euclid_2 = euclid (4, 2) == 2
ex_5 = euclid (13404, 8832)

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g [] = []
funkyMap f g [x] = [f x]
funkyMap f g (x:y:zs) = f x : g y : funkyMap f g zs

test_funky = funkyMap (+10) (+100) [1, 2, 3, 4, 5] == [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5]

ex_7 = sum $ funkyMap (+10) (+100) ys

ex_7' = sum $ funkyMap (\c -> if c == 'e' then 1 else 0) ord (poem >>= id)
