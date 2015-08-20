import Prelude hiding (Maybe, Just, Nothing)

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x-1, y)

type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m, n) = m*n

copy :: a -> Pair a
copy x = (x, x)

type Trans = Pos -> Pos

left' :: Trans
left' (x, y) = (x-1, y)

--type - złączenie istniejących typów
--data - definicja zupełnie nowego typu
--
--data Bool = True | False
--True i False to constructors typu Bool
--
--type and constructor names must begin with an UPPERCASE letter
--functions and type variables begin with lowercase letter

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float
           | Rectangle Float Float

-- :t Circle :: Float -> Shape
-- :t Rectangle :: Float -> Float -> Shape

square :: Float -> Shape
square n = Rectangle n n

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- RECURSIVE TYPES

--natural number
data Nat = Zero | Succ Nat
  deriving Show
--Zero :: Nat
--Succ :: Nat -> Nat

testNat = Succ (Succ (Succ Zero))
-- 3
-- 1 + (1 + (1 + 0))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2Int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add x y = int2nat (nat2int x + nat2int y)

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add m n)

testAdd = add (Succ (Succ Zero)) (Succ Zero)

data Expr = Val Int
          | Add Expr Expr
          | Mult Expr Expr
  deriving Show

size :: Expr -> Int
size (Val _) = 1
size (Add a b) = size a + size b
size (Mult a b) = size a + size b

eval :: Expr -> Int
eval (Val n) = n
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

fold :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
fold fval fadd fmult expr = choice expr
  where
    choice (Val n) = fval n
    choice (Add a b) = fadd (choice a) (choice b)
    choice (Mult a b) = fmult (choice a) (choice b)

eval' = fold id (+) (*)

testEval = eval (Add (Val 1) (Mult (Val 2) (Val 3))) == 7
testEval' = eval' (Add (Val 1) (Mult (Val 2) (Val 3))) == 7


--binary trees with ints in nodes and leafs
data Tree = Leaf Int
          | Node Tree Int Tree
  deriving Show

tree = Node (Node (Leaf 1) 3 (Leaf 4))
            5
            (Node (Leaf 6) 7 (Leaf 9))


--check if an integer is in a given tree
contains :: Int -> Tree -> Bool
contains m (Leaf n) = m==n
contains m (Node left n right) = m==n
                               || contains m left
                               || contains m right

testContains = contains 5 tree

flatten :: Tree -> [Int]
flatten (Leaf m) = [m]
flatten (Node left m right) = flatten left ++ [m] ++ flatten right

--a tree is a search tree if flattened to a list it is sorted

testFlatten = flatten tree

--since our tree is a search tree, we can optimize the contains function
contains' :: Int -> Tree -> Bool
contains' m (Leaf n) = m==n
contains' m (Node left n right) | m==n = True
                                | m<n = contains m left
                                | m>n = contains m right

testContains' = contains' 5 tree
