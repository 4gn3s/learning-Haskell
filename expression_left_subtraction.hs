module LeftSubt where
import Parser
import Data.Char

--expr -> expr - nat | nat
--nat -> 0 | 1 | ... | 9

expr = do d <- digit
          ds <- many (do char '-'
                         digit)
          return (foldl (-) d ds)
