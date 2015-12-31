data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
  show Nil = ""
  show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monad List where
  return x = Cons x Nil
  xs >>= k = join $ fmap k xs --this is the general bind definition

--join does list concatenation
join :: List (List a) -> List a
join Nil = Nil
join (Cons xs xss) = cat xs (join xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

-- l1 = Cons 1 (Cons 2 Nil)
-- l2 = Cons 3 Nil
neighbors :: (Num a) => a -> a -> List a
neighbors x dx = Cons (x - dx) (Cons x (Cons (x + dx) Nil))

--the fish operator: monadic functions composition
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = \x -> f x >>= g

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
--both formulations of the fish operator are equivalent

--monadic laws:
-- (f >=> g) >=> h = f >=> (g >=> h)
-- return >=> f = f
-- f >=> return = f
-- if you replace the fish with * and return with 1:
-- (a * b) * c = a * (b * c)
-- 1 * a = a
-- a * 1 = a

f x = [x, x+1]
g x = [x*x]

test = do
  x <- neighbors 0 100
  y <- neighbors x 1
  return y

test2 = g <=< f
test3 = f >=>g

main =
  print $ test
  -- print $ test2 7
  -- print $ test3 7
  -- print $ join $ Cons l1 (Cons l2 Nil)
