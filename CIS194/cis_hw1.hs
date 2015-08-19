--credit card number validation
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []--error "should be n>=0!"

test_toDigits_1 = toDigits 1234 == [1,2,3,4]
test_toDigits_2 = toDigits 0 == []
test_toDigits_3 = toDigits (-10) == []

test_toDigits = test_toDigits_1 && test_toDigits_2 && test_toDigits_3

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

test_toDigitsRev = toDigitsRev 1234 == [4,3,2,1]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

test_doubleEveryOther_1 = doubleEveryOther [1,2,3,4] == [1,4,3,8]
test_doubleEveryOther_2 = doubleEveryOther [8,7,6,5] == [8, 14, 6, 10]

test_doubleEveryOther = test_doubleEveryOther_1 && test_doubleEveryOther_2

listToDigits :: [Integer] -> [Integer]
listToDigits [] = []
listToDigits (x:xs) = toDigits x ++ listToDigits xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (listToDigits xs)

test_sumDigits = sumDigits [16,7,12,5] == 22

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0

test_validate_1 = validate 4012888888881881 == True
test_validate_2 = validate 4012888888881882 == False

test_validate = test_validate_1 && test_validate_2

------------------------------------------------------------
--the towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _= []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi_moves :: Integer -> Int
hanoi_moves n = length(hanoi n "a" "b" "c")

test_hanoi = hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

hanoi_n :: Int -> [Peg] -> [Move]
hanoi_n 0 _ = []
hanoi_n 1 (a:b:rest) = [(a,b)]
hanoi_n n (a:b:c:rest) = hanoi_n k (a:c:b:rest) ++ hanoi_n (n-k) (a:b:rest) ++ hanoi_n k (c:b:a:rest)
                         where k
                                | null rest = n - 1
                                | otherwise = n `quot` 2

test_hanoi_n_1 = hanoi_n 6 ["a", "b", "c", "d", "e"]
test_hanoi_n_2 = hanoi_n 6 ["a", "b", "c", "d"]
test_hanoi_n_3 = hanoi_n 8 ["a", "b", "c", "d", "e"]
