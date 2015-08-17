import Data.List (sort)
import Data.Set

type Party = String
type Ballot = [Party]

count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count a (x:xs)
		| a == x = 1 + count a xs
  		| otherwise = count a xs

test1 = count 'a' "ababca"

--rmdups :: (Eq a) -> [a] -> [a]
rmdups [] = []
rmdups (x:xs)
		| x `elem` xs = rmdups xs
  		| otherwise = x : rmdups xs

test2 = rmdups "ababca"

setRemove :: (Ord a) => [a] -> Set a -> [a]
setRemove [] _ = []
setRemove (x:xs) sofar
				| x `member` sofar = setRemove xs sofar
				| otherwise = x : (setRemove xs (insert x sofar))

rmdups' :: (Eq a) -> [a] -> [a]
rmdups' xs = setRemove xs empty
test3 = rmdups' "ababca"

frequency :: (Eq a) => [a] -> [(Int, a)]
frequency xs = [(count x xs, x) | x <- rmdups' xs]

test4 = frequency "ababca"
