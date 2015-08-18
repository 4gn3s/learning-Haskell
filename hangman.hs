module Hangman where

import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- secretlyGetLine
             putStrLn "Try to guess it"
             guess word

secretlyGetLine :: IO String
secretlyGetLine = do x <- getCh
                     if x == '\n' then
                       do putChar x
                          return []
                     else
                       do putChar '_'
                          xs <- secretlyGetLine
                          return (x:xs)

--gets char without printing it to screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar --c is not a mutable variable- the code just takes a result of the computation getChar and binds the result to c
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                  putStrLn "You WIN!"
                else
                  do putStrLn (diff word xs)
                     guess word

diff :: String -> String -> String
diff xs ys = [if x `elem` ys then x else '_' | x <- xs]
