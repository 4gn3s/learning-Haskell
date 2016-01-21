toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

test1 = toDigits 1234 == [1,2,3,4]
test2 = toDigitsRev 1234 == [4,3,2,1]
test3 = null (toDigits 0)
test4 = null (toDigits (-16))

doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x:y:xs) = x : 2*y : doubleSecond xs
doubleSecond (x:xs) = x : doubleSecond xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleSecond . reverse

test5 = doubleEveryOther [1,3,8,6] == [2,3,16,6]
test6 = doubleEveryOther [8,7,6,5] == [16,7,12,5]
test7 = doubleEveryOther [1,2,3] == [1,4,3]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

test8 = sumDigits [2,3,16,6] == 18
test9 = sumDigits [16,6,12,5] == (1+6+6+1+2+5)

validate :: Integer -> Bools
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

test10 = validate 4012888888881881
test11 = not (validate 4012888888881882)

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
--number of discs, names of 3 pegs, returns a list of moves
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
test12 = hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
