module Parser (Tree(..), parse) where

import Lexer
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
