fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

--returns the n-th fibonacci number counting from zero
fib :: Int -> Integer
fib n = fibs !! n

--get first fibonacci number greater than 1000
largerFib :: Integer
largerFib = head (dropWhile (<1000) fibs)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

repeatTree :: a -> Tree a
repeatTree a = Node t x t
  where
    t = repeatTree x
