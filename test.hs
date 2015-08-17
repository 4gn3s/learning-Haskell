import Prelude hiding (return)
type Parser a = String -> [(a, String)]

-- Just a helper function to aid readability later on
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

-- A parser that always succeeds
return :: a -> Parser a
return v = \imp -> [(v, imp)]

-- A parser that always fails
failure :: Parser a
failure = \_inp -> []

-- A simple parser that takes the first char
item :: Parser Char
item = \inp -> case inp of
               [] -> []
               (x:xs) -> [(x, xs)]

-- # Sequencing
-- the "then" operator
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(>>=) p f = \inp -> case parse p inp of
                      [] -> [] -- the expression evaluates to a fail if
                               -- the first parser fails
                      [(v, rest)] -> parse (f v) rest

-- # Choice
-- if the first parser fails, apply the second one instead.
(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = \inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, rest)] -> [(v, rest)]


-- # Derived primitives

-- ## sat: the satisfaction
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
           then return x
           else failure
