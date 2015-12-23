module Parser where

import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
    return v = Parser (\input -> [(v, input)])
    p >>= f = Parser (\input -> case parse p input of
                                [] -> []
                                [(v, out)] -> parse (f v) out)
    fail s = Parser (\input -> [])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) input = p input

failure :: Parser Char
failure = fail []

item :: Parser Char
item = Parser (\input -> case input of
                   [] -> []
                   (x:xs) -> [(x, xs)])

--concatente parsers: if the 1st parser succeddes, return the result
--otherwise go to the second parser and return its result
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\input -> case parse p input of
                                 [] -> parse q input
                                 [(v, out)] -> [(v, out)])

test1 = parse item "abc"
test2 = parse failure "abc"
test3 = parse (return 1) "abc"
test4 = parse (item +++ return 'd') "abc"
test5 = parse (failure +++ return 'd') "abc"

--read first element, skip second, return 1st and 3rd
p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x, y)

test6 = parse p "abcdef"

--parse if predicate p holds
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
               return x
           else
               failure

digit :: Parser Char
digit = sat isDigit

--parse a specific character x
char :: Char -> Parser Char
char x = sat (x ==)

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

--apply a parser 0 or more times (like regex)
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

--parsing a list of digits
listparser :: Parser String
listparser = do char '['
                d <- digit
                ds <- many (do char ','
                               digit)
                char ']'
                return (d:ds)

test7 = parse listparser "[1,2,3,4]"
test8 = parse listparser "[1,2,3"
