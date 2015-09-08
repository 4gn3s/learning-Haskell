import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM_ putStrLn args
    putStrLn $ "program name: " ++ progName
