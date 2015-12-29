module Lexer (Operator(..), Token(..), tokenize) where
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

--function using guards
operator :: Char -> Operator
operator c | c == '+' = Plus
		   | c == '-' = Minus
		   | c == '*' = Times
		   | c == '/' = Div

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
