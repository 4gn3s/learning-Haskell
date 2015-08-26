------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving (Eq, Show)

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (x :> _) = x

children :: Rose a -> [Rose a]
children (_ :> xs) = xs

test1 = root (1 :> [2 :> [], 3 :> []]) == 1
test2 = root ('a' :> []) == 'a'
test3 = children (1 :> [2 :> [], 3 :> []]) == [2 :> [], 3 :> []]
test4 = children ('a' :> []) == []

ex0_tree = 'x' :> map (flip (:>) []) ['a'..'x']
ex0 = length $ children ex0_tree

ex1_tree = 'x' :> map (\c -> c :> []) ['a'..'A']
ex1 = length (children ex1_tree)

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (x :> xs) = 1 + sum [size x' | x' <- xs]

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> xs) = sum $ map leaves xs

ex3_tree = 1 :> map (\c -> c :> []) [1..5]
ex3 = size ex3_tree

ex4_tree = 1 :> map (\c -> c :> []) [1..5]
ex4 = size . head . children $ ex4_tree

ex5_tree = 1 :> map (\c -> c :> []) [1..5]
ex5 = leaves ex5_tree

ex6_tree = 1 :> map (\c -> c :> []) [1..5]
ex6 = product (map leaves (children ex6_tree))

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (x :> xs) = (f x :> [fmap f x' | x' <- xs])

test5 = fmap (*2) (1 :> [2 :> [], 3 :> []]) == (2 :> [4 :> [], 6 :> []])
test6 = fmap (+1) (1 :> []) == (2 :> [])

ex8_tree = 1 :> map (\c -> c :> []) [1..5]
ex8 = size (fmap leaves (fmap (:> []) ex8_tree))

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

unSum :: Sum a -> a
unSum (Sum a) = a
unProduct :: Product a -> a
unProduct (Product a) = a

ex11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap f = fold . fmap f

flatten :: Rose a -> [a]
flatten (x :> []) = [x]
flatten (x :> xs) = x : (concat [flatten x' | x' <- xs])

instance Foldable Rose where
  fold tree = foldr mappend mempty $ flatten tree


ex14_tree = 1 :> [2 :> [], 3 :> [4 :> []]]
ex14_tree' = fmap Product ex14_tree
ex14 = unProduct $ fold ex14_tree'

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex16_tree = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
ex16 = unSum $ foldMap Sum ex16_tree

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum f = unSum (foldMap Sum f)
fproduct f = unProduct (foldMap Product f)

ex19 = fsum xs

ex20 = fproduct xs

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

