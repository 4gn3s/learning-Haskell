import System.Environment
import System.IO.Error
import System.IO
import System.Directory

main1 = do
    (fileName : _) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            putStrLn $ "File has " ++ show (length (lines contents)) ++ " lines"
        else
            putStrLn "file does not exist"

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    putStrLn $ "File has " ++ show (length (lines contents)) ++ " lines"

handler :: IOError -> IO ()
handler e = putStrLn "file does not exist"
