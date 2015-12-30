import Data.Char
import qualified Data.Map as M
import Control.Monad.State

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

type SymTab = M.Map String Double

type Evaluator a = State SymTab a

lookUp :: String -> Evaluator Double
lookUp str = do
    symTab <- get
    case M.lookup str symTab of
        Just v -> return v
        Nothing -> error $ "lookup failed for " ++ str


addSymbol :: String -> Double -> Evaluator ()
addSymbol str val = do
    symTab <- get
    put $ M.insert str val symTab
    return ()

evaluate :: Tree -> Evaluator Double

evaluate (SumNode op left right) = do
    l <- evaluate left
    r <- evaluate right
    case op of
        Plus -> return $ l + r
        Minus -> return $ l - r

evaluate (ProdNode op left right) = do
    l <- evaluate left
    r <- evaluate right
    case op of
        Times -> return $ l * r
        Div -> return $ l / r

evaluate (UnaryNode op tree) = do
    v <- evaluate tree
    case op of
        Plus -> return v
        Minus -> return (-v)

evaluate (NumNode x) = return x

evaluate (VarNode str) = lookUp str

evaluate (AssignNode str tree) = do
    val <- evaluate tree
    addSymbol str val
    return val

expr = AssignNode "x" (ProdNode Times (VarNode "pi")
                                (ProdNode Times (NumNode 4) (NumNode 6)))

main = print $ runState (evaluate expr) (M.fromList [("pi", pi)])
