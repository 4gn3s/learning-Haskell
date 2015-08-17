reversed_quicksort :: (Ord a) => [a] -> [a]
reversed_quicksort [] = []
reversed_quicksort (a:as) = reversed_quicksort bigger ++ [a] ++ reversed_quicksort smaller where
							bigger = [x | x <- as, x > a]
							smaller = [x | x <- as, x <= a]
