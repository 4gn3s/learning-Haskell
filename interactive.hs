module InteractiveIO where
import Prelude hiding (getLine, putStr, putStrLn)

--interactive programs have side effects
--e.g. readline is not a mathematical function- you call it with no arguments,
--but it always returns different results (it has side effects)
--
--the IO monad expresses in the type of a function, that it has side effects

--IO a
--action returning type a, that has side effects

--IO Char
--IO () --returns nothing, but has side effects - like returning void

--what do we already have in the IO Monad:
--getChar :: IO Char
--putChar :: Char -> IO ()
--return :: a -> IO a

a :: IO (Char, Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x, y)

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
               return []
             else
               do xs <- getLine
                  return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do putStrLn "Enter a string:"
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >>= \x -> putChar '\n'

getLine' = get []
get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f = do xs <- getLine'
                 putStrLn' (f xs)

--takes a list of monadic values and evaluates them into a sequence from left to right ignoring intermediate results
sequence_' :: Monad m -> [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

sequence_'' :: Monad m -> [m a] -> m ()
sequence_''[] = return ()
sequence_'' (m:ms) = m >> sequence_'' ms

sequence_''' :: Monad -> [m a] -> m ()
sequence_''' [] = return ()
sequence_''' (m:ms) = m >>= \_ -> sequence_''' ms

sequence_'''' :: Monad m -> [m a] -> m ()
sequence_'''' ms = foldr (>>) (return ()) ms




