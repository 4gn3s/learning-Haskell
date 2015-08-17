import Data.Char

evens :: [Int] -> [Int]
evens xs = [x | x <- xs, even x]

squares :: Int -> [Int]
squares n = [x*x | x <- [1..n]]

fiftySquares :: [Int]
fiftySquares = squares 50

coords :: Int -> Int -> [(Int, Int)]
coords m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = coords n n

square_no_diag :: Int -> [(Int, Int)]
square_no_diag n = [(x, y) | (x, y) <- square n, x /= y]

clean :: String -> String
clean ss = [toLower s | s <- ss, isAlpha s]
