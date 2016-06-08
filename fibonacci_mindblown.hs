fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

test = take 100 fibs
