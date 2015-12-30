module EvaluatorStateMonad (evaluate, Evaluator(..)) where

import Lexer
import Parser
import qualified Data.Map as M

type SymbTab = M.Map String Double

newtype Evaluator a = Ev (SymbTab -> (a, SymbTab))

-- instance Monad Evaluator where
-- 	(Ev act) >>= k = Ev $
-- 		\sym ->
-- 			let (x, sym') = act sym
-- 					(Ev act') = k x
-- 			in act' sym'
-- 	return v = Ev (\sym -> (v, sym))

instance Monad Evaluator where
    (Ev act) >>= k = Ev $
        \symTab ->
            let (x, symTab') = act symTab
                (Ev act') = k x
            in act' symTab'
    return x = Ev (\symTab -> (x, symTab))

lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
		case M.lookup str symTab of
			Just v -> (v, symTab)
			Nothing -> error $ "undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
	let symTab' = M.insert str val symTab
	in (val, symTab')

evaluate :: Tree -> Evaluator Double

evaluate (NumNode x) = return (fromIntegral x)

evaluate (SumNode op l r) = do
		left <- evaluate l
		right <- evaluate r
 		case op of
	 		Plus -> return $ left + right
			Minus -> return $ left - right

evaluate (ProdNode op l r) = do
		left <- evaluate l
		right <- evaluate r
		case op of
				Times -> return $ left * right
				Div -> return $ left / right

evaluate (UnaryNode op n) = do
		x <- evaluate n
		case op of
			Plus -> return x
			Minus -> return (-x)

evaluate (AssignNode str tree) = do
	 v <- evaluate tree
	 addSymbol str v

evaluate (VarNode str) = lookUp str
