module Evaluator (evaluate, Evaluator(..)) where

import Lexer
import Parser
import qualified Data.Map as M

type SymbTab = M.Map String Double

newtype Evaluator a = Ev (Either String a)

instance Monad Evaluator where
	(Ev ev) >>= k =
		case ev of
			Left msg -> Ev (Left msg)
			Right v -> k v
	return v = Ev (Right v)
	fail str = Ev (Left str)

lookUp :: String -> SymbTab -> Evaluator (Double, SymbTab)
lookUp str symTab =
		case M.lookup str symTab of
			Just v -> return (v, symTab)
			Nothing -> fail ("undefined variable " ++ str)

addSymbol :: String -> Double -> SymbTab -> Evaluator ((), SymbTab)
addSymbol str val symTab =
	let symTab' = M.insert str val symTab
	in return ((), symTab')

evaluate :: Tree -> SymbTab -> Evaluator (Double, SymbTab)

evaluate (NumNode x) symTab = return (fromIntegral x, symTab)

evaluate (SumNode op l r) symTab = do
		(left, symTab') <- evaluate l symTab
		(right, symTab'') <- evaluate r symTab'
 		case op of
	 		Plus -> return (left + right, symTab'')
			Minus -> return (left - right, symTab'')

evaluate (ProdNode op l r) symTab = do
		(left, symTab') <- evaluate l symTab
		(right, symTab'') <- evaluate r symTab'
		case op of
				Times -> return (left * right, symTab'')
				Div -> return (left / right, symTab'')

evaluate (UnaryNode op n) symTab = do
		(x, symTab') <- evaluate n symTab
		case op of
			Plus -> return (x, symTab')
			Minus -> return (-x, symTab')

evaluate (AssignNode str tree) symTab = do
	 (v, symTab') <- evaluate tree symTab
	 (_, symTab'') <- addSymbol str v symTab'
	 return (v, symTab'')

evaluate (VarNode str) symTab = lookUp str symTab
