module RecursiveTypes where

--simple list definition
---------------------------------------------------
data List' a = Empty'
             | Cons' { head :: a, tail :: List' a }
    deriving (Eq, Ord, Read, Show)
--cons is another word for :

test1 = 3 `Cons'` (4 `Cons'` (5 `Cons'` Empty'))

infixr 5 :-:
data List a = Empty
            | a :-: (List a)
    deriving (Eq, Ord, Read, Show)

a = 3 :-: 4 :-: 5 :-: Empty
test2 = a
test3 = 100 :-: a

--addding two lists together
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
xs .++ Empty = xs
(x :-: xs) .++ ys = x :-: (xs .++ ys)

b = 6 :-: 7 :-: Empty
test4 = a .++ b

--simple binary search tree
---------------------------------------------------
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
    deriving (Eq, Read, Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert :: Ord a => a -> Tree a -> Tree a
insert x EmptyTree = singleton x
insert x (Node y left right)
    | x == y = Node x left right
    | x < y = Node y (insert x left) right
    | x > y = Node y left (insert x right)

inTree :: Ord a => a -> Tree a -> Bool
inTree x EmptyTree = False
inTree x (Node y left right)
    | x == y = True
    | x < y = inTree x left
    | x > y = inTree x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr insert EmptyTree nums
test5 = numsTree
test6 = 8 `inTree` numsTree == True
test7 = 100 `inTree` numsTree == False

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

test8 = fmap (*2) EmptyTree
test9 = fmap (*4) (foldr insert EmptyTree [5,7,3,2,1,7])
