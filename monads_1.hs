module Monads1 where
--import Prelude hiding (Just, Nothing, Maybe)
import Prelude hiding ((>>=))
inc :: [Int] -> [Int]
inc [] = []
inc (x:xs) = (x + 1) : inc xs

sqr :: [Int] -> [Int]
sqr [] = []
sqr (x:xs) = x^2 : sqr xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

inc' = map' (+1)
sqr' = map' (^2)

data Expr = Val Int | Div Expr Expr

--simple lang of expressions with division operator
eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y
--but if we divide by 0, it throws an error

--data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv x y = if y == 0 then Nothing else Just (x `div` y)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of
                       Nothing -> Nothing
                       Just n -> case eval' y of
                                      Nothing -> Nothing
                                      Just m -> safediv n m

test1 = eval (Div (Val 8) (Val 4))
test2 = eval (Div (Val 10) (Val 0))
test3 = eval' (Div (Val 8) (Val 4))
test4 = eval' (Div (Val 10) (Val 0))

sequ :: Maybe a -> Maybe b -> Maybe (a, b)
sequ Nothing _ = Nothing
sequ _ Nothing = Nothing
sequ (Just x) (Just y) = Just (x, y)

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = apply f (eval'' x `sequ` eval'' y)
                   where f (n, m) = safediv n m

test5 = eval'' (Div (Val 8) (Val 4))

--sequencing operator "then"
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
(Just x) >>= f = f x

-- evaluate x and call it n, evaluate y and call it m, perform divison
eval''' :: Expr -> Maybe Int
eval''' (Val n) = Just n
eval''' (Div x y) = eval''' x >>=  (\n -> eval''' y >>= (\m -> safediv n m))

test6 = eval''' (Div (Val 8) (Val 4))
test7 = eval''' (Div (Div (Val 8) (Val 2)) (Val 2))

-- generally a typical structure built using >>= operator has the following
-- structure:
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- ..
-- mn >>= \xn ->
-- f x1 x2 .. xn

--which can be written in a cleaner form:
--do x1 <- m1
--   x2 <- m2
--   ..
--   xn <- mn
--   f x1 x2 .. xn

eval'''' :: Expr -> Maybe Int
eval'''' (Val n) = Just n
eval'''' (Div x y) = do n <- eval'''' x
                        m <- eval'''' y
                        safediv n m

test8 = eval'''' (Div (Val 8) (Val 4))
