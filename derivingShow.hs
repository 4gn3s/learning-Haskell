data Operator = Plus | Minus | Times | Div

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String

instance Show Operator where
    show Plus  = " + "
    show Minus = " - "
    show Times = " * "
    show Div = " / "

instance Show Tree where
    show (SumNode op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
    show (ProdNode op left right) = show left ++ show op ++ show right
    show (AssignNode str left) = str ++ " = " ++ show left
    show (UnaryNode op left) = show op ++ show left
    show (NumNode n) = show n
    show (VarNode str) = str

expr = AssignNode "x" (ProdNode Div (SumNode Minus (NumNode 13) (NumNode 1)) (VarNode "y"))

main = print expr
