import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
  deriving Show

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

natToInteger' :: Nat -> Integer
natToInteger' = \ n -> genericLength [c | c <- show n, c == 'S']

natToInteger'' :: Nat -> Integer
natToInteger'' = head . m
  where m Zero = [0]
        m (Succ x) = [sum [x| x <- (1:m x)]]

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = (Succ (integerToNat (n-1)))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

--data Ordering = LT | EQ | GT
data Tree = Leaf Integer | Node Tree Integer Tree
--compare :: (Ord a) => a -> a -> Ordering

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) =
  case compare m n of
    LT -> occurs m l
    GT -> occurs m r
    EQ -> True

occurs' :: Integer -> Tree -> Bool
occurs' m (Leaf n) = m==n
occurs' m (Node l n r)
    | m == n = True
    | m < n = occurs' m l
    | otherwise = occurs' m r

data LeafTree = LLeaf Integer | LNode LeafTree LeafTree
  deriving Show
--a tree is balanced if the number of leaves in the left and right subtree of
--every node differs by at most one, with leaves themselves being trivially balanced

leaves :: LeafTree -> Integer
leaves (LLeaf _) = 1
leaves (LNode l r) = leaves l + leaves r

balanced :: LeafTree -> Bool
balanced (LLeaf _) = True
balanced (LNode l r) = abs(leaves l - leaves r) <= 1 && balanced l && balanced r

halve :: [Integer] -> ([Integer], [Integer])
halve xs = splitAt (length xs `div` 2) xs

balance :: [Integer] -> LeafTree
balance [x] = LLeaf x
balance xs = LNode (balance ys) (balance zs)
  where
    (ys, zs) = halve xs
