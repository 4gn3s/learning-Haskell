import Data.List

solveRPN :: (Num a, Read a, Floating a) => String -> a
solveRPN = head . foldl foldFun [] . words where
    foldFun (x : y : ys) "*" = (y * x) : ys
    foldFun (x : y : ys) "/" = (y / x) : ys
    foldFun (x : y : ys) "+" = (y + x) : ys
    foldFun (x : y : ys) "-" = (y - x) : ys
    foldFun (x : y : ys) "^" = (y ** x) : ys
    foldFun (x : xs) "ln" = log x : xs
    foldFun xs "sum" = [sum xs]
    foldFun xs x = read x : xs


test1 = solveRPN "10 4 3 + 2 * -" == (-4)
test2 = solveRPN "2 3 +" == 5
test3 = solveRPN "90 34 12 33 55 66 + * - +" == (-3947)
test4 = solveRPN "2.7 ln"
test5 = solveRPN "10 10 10 10 sum 4 /" == 10
test6 = solveRPN "10 2 ^" == 100
test7 = solveRPN "43.2425 0.5 ^"
