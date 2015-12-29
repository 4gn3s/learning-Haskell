import Prelude hiding (span)
import Data.Char

data Operator = Plus | Minus | Times | Div
	deriving (Show, Eq)
data Token = TokOp Operator
		   | TokId String
	 	   | TokNum Int
	       | TokLPar
		   | TokRPar
		   | TokAssign
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

--lexer
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
	| elem c "+-*/" = (TokOp $ operator c) : tokenize cs
	| c == '('  = TokLPar : tokenize cs
	| c == ')'  = TokRPar : tokenize cs
	| c == '='  = TokAssign : tokenize cs
 	| isDigit c = number c cs
  	| isAlpha c = identifier c cs
   	| isSpace c = tokenize cs
 	| otherwise = error $ "Cannot tokenizeChar " ++ [c]

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
					  TokId (c:str) : tokenize cs'

number :: Char -> String -> [Token]
number c cs = let (digs, cs') = span isDigit cs in
				  TokNum (read (c:digs)) : tokenize cs'

-- alnum :: String -> (String, String)
-- alnum str = aln "" str
-- 	where
-- 	 aln acc [] = (acc, [])
--   	 aln acc (c:cs)
-- 			| isAlphaNum c =
-- 	   			let (acc', cs') = aln acc cs in (c:acc', cs')
-- 	   		| otherwise = (acc, c:cs)

-- digits :: String -> (String, String)
-- digits str = digs "" str
-- 	where
-- 	 digs :: String -> String -> (String, String)
--   	 digs acc [] = (acc, [])
-- 	 digs acc (c:cs)
--   			| isDigit c =
-- 		 		let (acc', cs') = digs acc cs in (c:acc', cs')
-- 		 	| otherwise = (acc, c:cs)


--span takes a predicate p and a list xs, and returns a tuple of lists
--the first element is the longest prefix of xs of elements which satisfy p
--the second element is the rest of xs
--span p xs is eqivalent to (takeWhile p xs, dropWhile p xs)
span :: (a -> Bool) -> [a] -> ([a], [a])
span pred str = spanAcc [] str
	where
  	 spanAcc acc [] = (acc, [])
	 spanAcc acc (c:cs)
  		| pred c =
			let (acc', cs') = spanAcc acc cs in (c:acc', cs')
	   	| otherwise = (acc, c:cs)

test1 = tokenize "1 + 3*x - 22"
test2 = print $ span isAlphaNum "R2D2+C3P0"
test3 = tokenize "r2d2+cpo0-11"
test4 = tokenize "x=(a+2)*b-7"

-- deSpace :: [Token] -> [Token]
-- deSpace = filter (\t -> t /= TokSpace)

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
