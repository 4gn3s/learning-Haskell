factorial :: Integer -> Integer
factorial n
        | n == 0 = 1
        | n > 0 = n * factorial (n-1)

fibonacci :: Integer -> Integer
fibonacci n
        | n == 0 = 1
        | n == 1 = 1
        | n > 1 = fibonacci (n-2) + fibonacci (n-1)
