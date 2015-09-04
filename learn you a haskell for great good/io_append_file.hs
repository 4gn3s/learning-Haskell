import System.IO

main = do
    todoItem <- getLine
    appendFile "todos.txt" (todoItem ++ "\n")
