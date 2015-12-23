data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                 deriving (Eq, Ord)

listToBinTree :: (Ord a) => [a] -> BinTree a
listToBinTree [] = Empty
listToBinTree (x:xs) = Node x (listToBinTree (filter (<x) xs))
                              (listToBinTree (filter (>x) xs))

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

test1 = print $ listToBinTree [7, 2, 4, 8, 1, 3, 6, 21, 12, 23]
test2 = print $ listToBinTree ["yo", "dawg", "asdf", "django", "python my love"]

nullTree = Node 0 nullTree nullTree

makeTreeInternal _ _ Empty = Empty
makeTreeInternal _ 0 _ = Empty
makeTreeInternal level n (Node x left right) =
			let
				nl = makeTreeInternal (level+1) (n-1) left
   				nr = makeTreeInternal (level+1) (n-1) right
	  		in
	 			Node level nl nr

makeTreeDepth = makeTreeInternal 0

test3 = print $ makeTreeDepth 4 nullTree
