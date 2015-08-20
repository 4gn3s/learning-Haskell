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

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

--expression is a value or an application of an operator
data Expr = Val Int | App Op Expr Expr

--eval either:
--succeeds and returns a singleton list
--fails and returns an empty list
--(lists are cleaner than Maybe type)
eval :: Expr -> [Int]
eval (Val a) = [a | a>0]
eval (App o l r) = [apply o x y | x <- eval l,
                                y <- eval r,
                                valid o x y]

choices :: [a] -> [[a]]
--choices [1,2] = [[], [1], [2], [1,2], [2,1]]
insert x xs = [[x]] ++ [xs] ++ [(take n xs) ++ [x] ++ (drop n xs) | n <- [0..(length xs)]]

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
--split [1,2,3,4] = [([1], [2,3,4]), ([1,2], [3,4]), ([1,2,3], [4])]
split [] = error "can't split an empty list"
split xs = [(take n xs, drop n xs) | n <- [1..(length xs - 1)]]

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

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]
