module Expr where
import Parser
import Data.Char
--expr -> term '+' expr | term
--term -> factor '*' term | factor
--factor -> digit | '(' expr ')'
--digit -> '0' | '1' | ... | '9'
--
--expr -> term ('+' expr | eps)
--term -> factor ('*' term | eps)

expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             return (t + e)
           +++ do char '-'
                  e <- expr
                  return (t - e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             return (f * t)
           +++ do char '/'
                  t <- term
                  return (f `div` t)
           +++ return f

factor' :: Parser Int
factor' = do d <- digit
             return (digitToInt d)
           +++ do char '('
                  e <- expr
                  char ')'
                  return e

factor :: Parser Int
factor = do char '('
            e <- expr
            char ')'
            return e
          +++ int

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])] -> n
               [(_, out)] -> error ("ununsed input " ++ out)
               [] -> error ("invalid input " ++ xs)
--eval xs = fst (head (parse expr xs))

test1 = eval "2*3+4"
test2 = eval "2*(3+4)"
test3 = eval "2*(4-3)"
test4 = eval "(5+4)/3"
test5 = eval "10/2+4"
