import Prelude hiding (and, or, not)

-- 3.01 (**) Truth tables for logical expressions.
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

not :: Bool -> Bool
not False = True
not True = False

nand :: Bool -> Bool -> Bool
nand x y = not (and x y)

nor :: Bool -> Bool -> Bool
nor x y = not (or x y)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

equ :: Bool -> Bool -> Bool
equ True True = True
equ False False = True
equ _ _ = False

impl :: Bool -> Bool -> Bool
-- a implies b
-- equivalent to (not a) or (b)
impl x y = (not x) `or` y

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
    | a <- [True, False], b <- [True, False]]

-- 3.02 (*) Truth tables for logical expressions (2).
-- Haskell has infix notation for this
