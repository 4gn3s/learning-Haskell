import Prelude hiding (sum, map, filter)
data List a = Cons a (List a) | Empty

singleton :: List a -> Bool
singleton (Cons _ Empty) = True
singleton _ = False

sum :: Num a => List a -> a
sum (Cons x xs) = x + sum xs
sum Empty = 0

lista = Cons 2 $ Cons 4 $ Cons 5 Empty

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs
main = do
	print $ singleton Empty
	print $ singleton $ Cons 2 Empty
	print $ singleton $ Cons 2 $ Cons 3 Empty
 	print $ sum lista
