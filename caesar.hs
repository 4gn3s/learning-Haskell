import Data.Char

let2int :: Char -> Char -> Int
let2int c a = ord c - ord a

int2let :: Int -> Char -> Char
int2let n a = chr (ord a + n)

shiftCase :: Int -> Char -> Char -> Char
shiftCase n c a = int2let ((let2int c a + n) `mod` 26) a

shift :: Int -> Char -> Char
shift n c
	| isSpace c = c
	| isLower c = shiftCase n c 'a'
  	| otherwise = shiftCase n c 'A'

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
