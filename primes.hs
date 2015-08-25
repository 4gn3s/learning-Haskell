--simple procedure to generate an infinite list of primes:
--1. create list [2,3,4...]
--2. mark the first value p in the list as prime
--3. delete all multiplies of p from the list
--4. goto 2.
--(the sieve of eratosthenes)

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

testPrimes = take 100 primes
