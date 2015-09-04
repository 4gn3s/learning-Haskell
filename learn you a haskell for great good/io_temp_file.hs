import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "todos.txt" ReadMode
    (tmpName, tmpHandle) <- openTempFile "." "temp"
    --openTempFile takes a path to a temporary directory and a template name for a file and opens a temporary file
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "Current TODOs:"
    putStr $ unlines numberedTasks
    putStrLn "Which to delete?"
    numberStr <- getLine
    let number = read numberStr
        newTodoList = delete (todoTasks !! number) todoTasks
        --delete and !! functions from Data.List
        -- !! returns an element from a list with some index
        --delete deletes the first occurence of an element in a list and returns a new list without that occurence
    hPutStr tmpHandle $ unlines newTodoList
    hClose handle
    hClose tmpHandle
    removeFile "todos.txt"
    renameFile tmpName "todos.txt"
    --removeFile and renameFile are both in System.Directory
    --and both take file paths as parameters (not handles)
