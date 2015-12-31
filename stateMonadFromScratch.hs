data Tree a = Leaf a
            | Node (Tree a) (Tree a)
    deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

type State = Int
data ST a = S (State -> (a, State))

apply :: ST a -> State -> (a, State)
apply (S f) x = f x

instance Monad ST where
  return x = S (\s -> (x, s))
  st >>= f = S (\s ->
        let (x, s') = apply st s
        in apply (f x) s')

fresh :: ST Int
fresh = S (\n -> (n, n+1))

app :: (State -> State) -> ST State
app f = S (\n -> (n, f n))

fresh' = app (+1)

run :: ST a -> State -> a
run x v = fst $ apply x v

label' t = run (mlabel t) 0

mlabel :: Tree a -> ST (Tree (a, Int))
mlabel (Leaf l) = do
  n <- fresh'
  return (Leaf (l, n))
mlabel (Node left right) = do
  l <- mlabel left
  r <- mlabel right
  return (Node l r)

label :: Tree a -> Tree (a, Int)
label t = fst $ apply (mlabel t) 0

test = label' tree
