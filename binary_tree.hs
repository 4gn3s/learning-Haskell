shuffle = map (\x -> (x*3123) `mod` 4331) [1..] --pseudo random list of numbers

-- a better shuffle function without the lower and the upper bound
new_shuffle = map rand [1..]
		where
	  		rand x = ((p x) `mod` (x+c)) - ((x+c) `div` 2)
			p x = m*x^2 + n*x + o
   			m = 3123
	  		n = 31
	 		o = 7641
			c = 1237

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                 deriving (Eq, Ord)

-- a problem in this function: filter (<1) [2..] will go forever, because it's not smart enough to figure out that it's an empty set
listToBinTree :: (Ord a) => [a] -> BinTree a
listToBinTree [] = Empty
listToBinTree (x:xs) = Node x (listToBinTree (filter (<x) xs))
                              (listToBinTree (filter (>x) xs))

safeListToBinTree :: (Ord a) => [a] -> BinTree a
safeListToBinTree [] = Empty
safeListToBinTree (x:xs) = Node x (safeListToBinTree (safefilter (<x) xs))
								  (safeListToBinTree (safefilter (>x) xs))

--if we can't find the elem after tryNumber consecutive steps, we assume it does not exist
safefilter :: (a -> Bool) -> [a] -> [a]
safefilter f xs = safefilter' f xs tryNumber
	where
	 	tryNumber = 1000
   		safefilter' _ _ 0 = []
	 	safefilter' _ [] _ = []
   		safefilter' f (x:xs) n = if f x
								 	then x : safefilter' f xs tryNumber
		  						 else
		   							safefilter' f xs (n-1)

instance (Show a) => Show (BinTree a) where
	show t = "<" ++ replace '\n' "\n: " (treeshow "" t)
		where
			treeshow pref Empty = "" --empty
			treeshow pref (Node x Empty Empty) = (pshow pref x) --leaf
			treeshow pref (Node x left Empty) = (pshow pref x) ++ "\n" ++ (showChild pref "`--" "    " left)
			treeshow pref (Node x Empty right) = (pshow pref x) ++ "\n" ++ (showChild pref "`--" "    " right)
			treeshow pref (Node x left right) = (pshow pref x) ++ "\n" ++ (showChild pref "|--" "|    " left) ++ "\n" ++ (showChild pref "`--" "     " right)
			showChild pref before next t = pref ++ before ++ treeshow (pref ++ next) t
			pshow pref x = replace '\n' ("\n" ++ pref) (show x)
			replace c new string =
				concatMap (change c new) string
				where
					change c new x
						| x == c = new
						| otherwise = x : []

test0 = print $ listToBinTree (take 10 [1..])
test1 = print $ listToBinTree [7, 2, 4, 8, 1, 3, 6, 21, 12, 23]
test2 = print $ listToBinTree ["yo", "dawg", "asdf", "django", "python my love"]

nullTree = Node 0 nullTree nullTree

makeTreeDepth _ Empty = Empty
makeTreeDepth 0 _ = Empty
makeTreeDepth n (Node x left right) =
			let
				nl = makeTreeDepth (n-1) left
   				nr = makeTreeDepth (n-1) right
	  		in
	 			Node x nl nr

test3 = print $ makeTreeDepth 4 nullTree
test4 = print $ makeTreeDepth 4 (listToBinTree shuffle)
test5 = print $ makeTreeDepth 4 (listToBinTree new_shuffle)
test6 = print $ makeTreeDepth 100 (safeListToBinTree new_shuffle)
