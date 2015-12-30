class Valuable a where
	evaluate :: a -> Double

data Expr = Const Double | Add Expr Expr

instance Valuable Expr where
	evaluate (Const d) = d
 	evaluate (Add a b) = evaluate a + evaluate b

instance Valuable Bool where
	evaluate True = 1
 	evaluate False = 0

test :: Valuable a => a -> IO()
test v = print $ evaluate v

expr :: Expr
expr = Add (Const 2) (Add (Const 1.5) (Const 8))

main = do
	test expr
 	test True
