module Evaluator (evaluate) where

import Lexer
import Parser
import qualified Data.Map as M

type SymbTab = M.Map String Double

lookUp :: String -> SymbTab -> Either String (Double, SymbTab)
lookUp str symTab =
		case M.lookup str symTab of
			Just v -> Right (v, symTab)
			Nothing -> Left ("undefined variable " ++ str)

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
evaluate (VarNode str) symTab =
	case lookUp str symTab of
		Left str -> error str
		Right (v, symTab) -> (v, symTab)
