import Data.Char

data Operator = Plus | Minus | Times | Div
	deriving (Show, Eq)
data Token = TokOp Operator
		   | TokId String
	 	   | TokNum Int
	  	   | TokSpace
	  deriving (Show, Eq)
data Expression

opToStr :: Operator -> String
opToStr Plus = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div = "/"

--function using guards
operator :: Char -> Operator
operator c | c == '+' = Plus
		   | c == '-' = Minus
		   | c == '*' = Times
		   | c == '/' = Div

showContent :: Token -> String
showContent (TokOp op) = opToStr op
showContent (TokId id) = id
showContent (TokNum n) = show n

--isDigit :: Char -> Bool
--isDigit c = elem c "0123456789"

--tokenize a single character
tokenizeChar :: Char -> Token
tokenizeChar c
	| elem c "+-*/" = (TokOp $ operator c)
 	| isDigit c = (TokNum $ digitToInt c)
  	| isAlpha c = (TokId [c])
   	| isSpace c = TokSpace
 	| otherwise = error $ "Cannot tokenizeChar " ++ [c]

test1 = deSpace $ tokenize "1 + 3*x - 22"

deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokSpace)

tokenize :: String -> [Token]
tokenize = map tokenizeChar

parse :: [Token] -> Expression
parse ts = undefined

evaluate :: Expression -> Double
evaluate expr = undefined

main :: IO()
main = do
	line <- getLine
 	putStrLn line
  	print (TokOp Plus)
   	print $ operator '*'
  	main
