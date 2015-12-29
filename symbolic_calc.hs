import Prelude hiding (span)
import qualified Data.Map as M --qualified means that the namespace is accessible through Data.Map.<name>
import Data.Char

data Operator = Plus | Minus | Times | Div
	deriving (Show, Eq)
data Token = TokOp Operator
					 | TokId String
					 | TokNum Int
					 | TokLPar
					 | TokRPar
					 | TokAssign
					 | TokEnd
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

--the grammar
--Expression <- Term [+-] Expression
--			 |  Identifier '=' Expression
--			 |  Term
--Term <- Factor [*/] Term
--     |  Factor
--Factor <- Number
--       |  Identifier
--       |  [+-] Factor
--       |  '(' Expression ')'

data Tree = SumNode Operator Tree Tree
				  | ProdNode Operator Tree Tree
				  | AssignNode String Tree
				  | UnaryNode Operator Tree
				  | NumNode Int
				  | VarNode String
	deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (x:xs) = xs

expression :: [Token] -> (Tree, [Token])
expression toks =
	let (termTree, toks') = term toks
	in
 		case lookAhead toks' of
	 		(TokOp op) | elem op [Plus, Minus] ->
				let (exTree, toks'') = expression (accept toks')
				in (SumNode op termTree exTree, toks'')
			TokAssign ->
	   			case termTree of
		 			VarNode str ->
		  				let (exTree, toks'') = expression (accept toks')
						in (AssignNode str exTree, toks'')
	  				_ -> error "you can only assign to variables"
	   		_ -> (termTree, toks')


term :: [Token] -> (Tree, [Token])
term toks =
	let (factorTree, toks') = factor toks
	in
 		case lookAhead toks' of
	 		(TokOp op) | elem op [Times, Div] ->
				let (termTree, toks'') = term (accept toks')
				in (ProdNode op factorTree termTree, toks'')
			_ -> (factorTree, toks')


factor :: [Token] -> (Tree, [Token])
factor toks =
	case lookAhead toks of
			(TokNum x) -> (NumNode x, accept toks)
			(TokId str) -> (VarNode str, accept toks)
			(TokOp op) | elem op [Plus, Minus] ->
		 		let (factorTree, toks') = factor (accept toks)
				in (UnaryNode op factorTree, toks')
			TokLPar ->
		 		let (exTree, toks') = expression (accept toks)
				in
   				if lookAhead toks' /= TokRPar
		  		then error "Missing right parenthesis"
	  			else (exTree, accept toks')
			_ -> error $ "Parse error on token" ++ show toks

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
			 in
				if null toks'
	   			then tree
	   			else error $ "leftover tokens" ++ show toks'

test5 = (parse . tokenize) "x1 = -15 / (x2 + 2)"

type SymbTab = M.Map String Double

lookUp :: String -> SymbTab -> (Double, SymbTab)
lookUp str symTab =
		case M.lookup str symTab of
			Just v -> (v, symTab)
			Nothing -> error $ "undefined variable " ++ str

addSymbol :: String -> Double -> SymbTab -> ((), SymbTab)
addSymbol str val symTab =
	let symTab' = M.insert str val symTab
	in ((), symTab')

evaluate :: Tree -> SymbTab -> (Double, SymbTab)
evaluate (NumNode x) symTab = (fromIntegral x, symTab)
evaluate (SumNode op l r) symTab =
	let (left, symTab') = evaluate l symTab
	    (right, symTab'') = evaluate r symTab'
	in
 		case op of
	 		Plus -> (left + right, symTab'')
			Minus -> (left - right, symTab'')
evaluate (ProdNode op l r) symTab =
	let (left, symTab') = evaluate l symTab
	    (right, symTab'') = evaluate r symTab'
   	in
		case op of
			Times -> (left * right, symTab'')
   			Div -> (left / right, symTab'')
evaluate (UnaryNode op n) symTab =
	let (x, symTab') = evaluate n symTab
	in
 		case op of
	 		Plus -> (x, symTab')
			Minus -> (-x, symTab')
evaluate (AssignNode str tree) symTab =
	let (v, symTab') = evaluate tree symTab
	    (_, symTab'') = addSymbol str v symTab'
	in (v, symTab'')
evaluate (VarNode str) symTab = lookUp str symTab

test6 = (evaluate . parse . tokenize) "x1 = -15 / ((28 + 2) *2)"

main :: IO()
main = do
	loop (M.fromList[("pi", pi), ("e", exp 1)])

loop symTab = do
	str <- getLine
	if null str
	then return ()
	else
		let toks = tokenize str
		    tree = parse toks
		    (val, symTab') = evaluate tree symTab
		in
				do
						print val
						print symTab'
						loop symTab'
