import System.Environment
import System.Directory
import System.IO
import Data.List

--todo list simple editor
--the commands available:
--ADD: todo add todo.txt "Find the magic sword of power"
--REMOVE: todo remove todo.txt 2
--VIEW: todo view todo.txt

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove)]

add :: [String] -> IO ()
add [fileName, text] = appendFile fileName (text ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let tasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, index] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read index
        tasks = lines contents
        newTasks = delete (tasks !! number) tasks
    hPutStr tempHandle $ unlines newTasks
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

main = do
    (command : args) <- getArgs
    --if we run: todo add todo.txt "Spank the monkey"
    --command will be "add" and args will be ["todo.txt", "Spank the monkey"]
    let (Just action) = lookup command dispatch
    action args
