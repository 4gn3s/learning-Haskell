import Data.Char
import Control.Monad

main = do
    return ()
    return "HAHAHA"
    --Using return doesn't cause the I/O do block to end in execution or anything like that
    --All these returns do is that they make I/O actions that don't really do anything except have an
    --encapsulated result and that result is thrown away because it isn't bound to a name
    line <- getLine
    return 4
    putStrLn line
    a <- return "hell"
    b <- return "yeah"
    putStrLn $ a ++ " " ++ b
    let aa = "hell"
        bb = "yeah"
    putStrLn $ aa ++ " " ++ bb
    putStr "ASDF"
    putStrLn "kontynuacja linii"
    putChar 'a'
    --print takes a value of any type that's an instance of Show
    --calls show with that value to stringify it and then outputs that string to the terminal
    print True
    print 2
    print "ASDF"
    print [1,2,3,4]
    rs <- sequence [getLine, getLine, getLine] --instead of doing three separate getLines
    print rs

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

--sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other
test1 = sequence $ map print [1..5]

--mapM takes a function and a list, maps the function over the list and then sequences it
test2 = mapM print [1..5]

--mapM_ does the same, only it throws away the result later
test3 = mapM_ print [1..5]

--forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever
testForever = forever $ do
    putStr "give me the input"
    l <- getLine
    putStrLn $ map toUpper l

--forM (located in Control.Monad) is like mapM, only that it has its parameters switched around.
--The first parameter is the list and the second one is the function to map over that list, which is then sequenced
testForM = do
    colors <- forM [1..3] (\a -> do
        putStrLn $ "give the color to associate with the number " ++ show a
        color <- getLine
        return color)
    putStrLn "the colors are"
    mapM_ putStrLn colors
