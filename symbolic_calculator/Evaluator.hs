module Evaluator (evaluate) where

import Lexer
import Parser
import qualified Data.Map as M

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
