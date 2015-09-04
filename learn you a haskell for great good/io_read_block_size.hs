import System.IO

main = do
    withFile "haiku.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        --by default, text files are read line by line
        --we can specify the chunk size using the hSetBuffering function.
        --It takes a handle and a BufferMode and returns an I/O action that sets the buffering
        --BufferMode is a simple enumeration data type and the possible values
        --it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int)
        --if we choose Maybe Int with Nothing, then the operating system determines the chunk size
        --NoBuffering means that it will be read one character at a time
        contents <- hGetContents handle
        putStr contents)
