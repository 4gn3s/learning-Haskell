verbose :: Bool -> Bool
--verbose b = if b == True then True else False
verbose b = b

complex :: x -> y -> (Int, y)
--complex x y = (fst p, snd q) where
--				p = head [(2, 3), (8, 4), (1, 5)]
--				q = (x, y)
complex x y = (2, y)

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

xor' :: Bool -> Bool -> Bool
xor' x y = if x then if not y then True else False else y

xor'' :: Bool -> Bool -> Bool
xor'' x y = x /= y

third :: [a] -> a
--third xs = xs !! 2
--
third (x1:x2:x3:xs) = x3
third _ = error "no third element"
--
--third xs = head (tail (tail xs))
