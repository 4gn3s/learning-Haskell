data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
    
leaf x = Node x Empty Empty

tree = Node 'a' (Node 'b' (Node 'd' Empty Empty) (Node 'e' Empty Empty)) (Node 'c' Empty (Node 'f' (Node 'g' Empty Empty) Empty))
tree' = Node 'a' (Node 'b' (leaf 'd') (leaf 'e')) (Node 'c' Empty (Node 'f' (leaf 'g') Empty))

-- 4.01 (*) Check whether a given term represents a binary tree
-- with this data type, it's impossible to build anything else

-- 4.02 (**) Construct completely balanced binary trees
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, 
-- which means their difference is not greater than one.

buildBalanced :: Int -> [Tree Int]
buildBalanced 0 = [Empty]
buildBalanced n = 
