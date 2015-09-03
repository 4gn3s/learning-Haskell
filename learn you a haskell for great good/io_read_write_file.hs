import System.IO
import Data.Char

main1 = do
    contents <- readFile "haiku.txt"
    --readFile :: FilePath -> IO String
    putStr contents

main = do
    contents <- readFile "haiku.txt"
    writeFile "haikuCAPS.txt" $ map toUpper contents
    --writeFile :: FilePath -> String -> IO ()


