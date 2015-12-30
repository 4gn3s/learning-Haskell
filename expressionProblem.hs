class Expr a
data Const = Const Double
data Add a b = Add a b
data Mul a b = Mul a b

--let's make const, add and mul instances of expr:
instance Expr Const
instance (Expr a, Expr b) => Expr (Add a b) --Both a and b must be Expr for Add a b to be Expr
instance (Expr a, Expr b) => Expr (Mul a b)

class (Expr e) => Valuable e where -- only expressions can be evaluated
	evaluate :: e -> Double

--now making const, add and mul instances of valuable as well
instance Valuable Const where
	evaluate (Const d)  = d
instance (Valuable a, Valuable b) => Valuable (Add a b) where
	evaluate (Add left right) = evaluate left + evaluate right
instance (Valuable a, Valuable b) => Valuable (Mul a b) where
  evaluate (Mul left right) = evaluate left * evaluate right

--adding new function to expressions:
class (Expr e) => Pretty e where
  prettyPrint :: e -> String

instance Pretty Const where
  prettyPrint (Const d) = show d
instance (Pretty a, Pretty b) => Pretty (Add a b) where
  prettyPrint (Add left right) = "(" ++ prettyPrint left ++ " + " ++ prettyPrint right ++ ")"
instance (Pretty a, Pretty b) => Pretty (Mul a b) where
  prettyPrint (Mul left right) = prettyPrint left ++ " * " ++ prettyPrint right

expr = Mul (Const 2) (Add (Const 1.5) (Const 2.5))

main = do
  putStrLn $ prettyPrint expr
  print $ evaluate expr
