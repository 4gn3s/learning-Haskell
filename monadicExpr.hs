data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Monad Expr where
  -- return :: a -> Expr a
  return x = Var x

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var a) >>= f = f a
  (Val n) >>= f = Val n
  (Add left right) >>= f = Add (left >>= f) (right >>= f)

e :: Expr Char
e  = Val 10 `Add` Val 20 `Add` Var 'a'
