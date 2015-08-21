--coutndown problem
--we are given a list of numbers [1, 3, 7, 10, 25, 50]
--and a list of operators [+, -, *, /]
--and a single value 765
--we should construct an expression using given numbers and operators
--whose value is equal 765
--additional rules:
--each number from the additional set can be used at most ONCE
--all intermediate results must be positive natural
--example: (25-10)*(50+1)=765
--there are 780 possible solutions for this example
--if we change the desired value to 831, there are no solutions at all

data Op = Add |
          Sub |
          Mul |
          Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--is the expression valid (are results positive and natural)
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

--expression is a value or an application of an operator
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App o l r) = show l ++ " " ++ show o ++ " " ++ show r

--eval either:
--succeeds and returns a singleton list
--fails and returns an empty list
--(lists are cleaner than Maybe type)
eval :: Expr -> [Int]
eval (Val a) = [a | a>0]
eval (App o l r) = [apply o x y | x <- eval l,
                                y <- eval r,
                                valid o x y]

--generowanie wszystkich zbiorow z elementów bez zmiany kolejnosci
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = xxs ++ map (x:) xxs where xxs = subs xs

testSubs = subs [1,2,3] == [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

--wstawienie elementu x na każdą możliwą pozycję w xs
insert :: a -> [a] -> [[a]]
insert x xs = [(take n xs) ++ [x] ++ (drop n xs) | n <- [0..(length xs)]]

testInsert = insert 4 [1,2,3] == [[4,1,2,3],[1,4,2,3],[1,2,4,3],[1,2,3,4]]

--concatMap: Map a function over all the elements of a container and concatenate the resulting lists.
--perms: wygenerowanie wszystkich możliwych permutacji ustawień elementów zbiorów
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insert x) . perms $ xs

testPerms = perms [1,2,3] == [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

choices :: [a] -> [[a]]
--choices [1,2] = [[], [1], [2], [1,2], [2,1]]
choices = (concatMap perms) . subs
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

values :: Expr -> [Int]
values (Val a) = [a]
values (App _ l r) = values l ++ values r

--given an expression, a list of integers and a value,
--is the evaluated expression a solution to the problem?
--
--1. the evaluated expression must be equal to the number
--2. are all the values in the expression in the list 'choices'
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

--all the possible ways to split a list to a pair of lists
--excluding empty lists
split :: [a] -> [([a], [a])]
split [] = error "can't split an empty list"
split xs = [(take n xs, drop n xs) | n <- [1..(length xs - 1)]]

split' :: [a] -> [([a], [a])]
split' [] = []
split' (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

testSplit = split [1,2,3,4] == [([1], [2,3,4]), ([1,2], [3,4]), ([1,2,3], [4])]

--brute force generation of all possible expressions
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

testCombine = combine (Val 10) (Val 20)

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

ex :: Expr
ex = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
testEval = eval ex == [765]

testSolution = solution ex [1,3,7,10,25,50] 765 == True

getAllSolutions = solutions [1,3,7,10,25,50] 765

type Result = (Expr, Int)

--zwraca liste par (wyrazenie, wynik) dla wszystkich mozliwych rozmieszczen operatorow w ciagu ns
--kolejnosc wyrazow ns nie jest zmieniana, zawsze wykorzystywane jest cale ns
results :: [Int] -> [Result]
results ns = [(e, n) | e <- exprs ns,
                       n <- eval e]

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns,
                     l <- results' ls,
                     r <- results' rs,
                     res <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div], valid' o x y]

testCombine' = combine' ((Val 10), 10) ((Val 20), 20)

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results' ns',
                       m == n]

--can we do better? yes, taking advantage of the fact that:
--x * y = y * x
--x * 1 = x

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1

--removes the first occurence of a given element from the list
removeone :: (Eq a) => a -> [a] -> [a]
removeone x [] = []
removeone x (y:ys)
            | x == y = ys
            | otherwise = y : removeone x ys

--are all elements in xs present in ys?
isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)
