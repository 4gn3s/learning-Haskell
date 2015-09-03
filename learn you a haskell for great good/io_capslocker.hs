import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l


--to run and test:
--ghc --make io_capslocker
--cat haiku.txt | ./io_capslocker
