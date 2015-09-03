import System.IO

main = do
    handle <- openFile "haiku.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

--to run: runhaskell io_readfile.hs
