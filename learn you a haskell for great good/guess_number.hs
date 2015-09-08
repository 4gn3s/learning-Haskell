import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNr gen

askForNr :: StdGen -> IO ()
askForNr gen = do
    let (randNr, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "which nr from 1 to 10 am i thinking of?"
    numberStr <- getLine
    when (not $ null numberStr) $ do
        let number = read numberStr
        if randNr == number then
            putStrLn "SUCCESS"
        else
            putStrLn $ "it was " ++ show randNr
        askForNr newGen
